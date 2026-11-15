from pathlib import Path
import xarray as xr
import arviz as az
import numpy as np
import pandas as pd
import pymc as pm
from scipy import stats
from scipy.optimize import brentq


folder = Path(__file__).resolve().parent


def fit_model(name, y, X, groups, refit=False):
    cache_file = folder / f"{name}.nc"
    if cache_file.exists() and not refit:
        print(f"Using saved fit: {cache_file.name}")
        return az.from_netcdf(cache_file)

    X_centered = X - X.mean()
    intercept_prior = round(float(np.median(y)), 1)

    with pm.Model(coords={"predictor": X.columns.tolist()}):
        # brms priors on the workbook scale
        intercept = pm.StudentT("intercept", nu=3, mu=intercept_prior, sigma=2.5)
        beta = pm.Flat("beta", dims="predictor")
        mu = intercept + X_centered.to_numpy() @ beta

        for group, labels, Z in groups:
            idx, levels = pd.factorize(labels)
            n_terms = Z.shape[1]
            z = pm.Normal(f"z_{group}", 0, 1, shape=(len(levels), n_terms))
            if n_terms == 1:
                sd = pm.HalfStudentT(f"sd_{group}", nu=3, sigma=2.5)
                effects = z * sd
            else:
                chol, _, _ = pm.LKJCholeskyCov(
                    f"cov_{group}", n=n_terms, eta=1,
                    sd_dist=pm.HalfStudentT.dist(nu=3, sigma=2.5)
                )
                effects = z @ chol.T
            mu = mu + (effects[idx] * Z).sum(axis=1)

        sigma = pm.HalfStudentT("sigma", nu=3, sigma=2.5)
        pm.Normal("likelihood", mu=mu, sigma=sigma, observed=y)
        idata = pm.sample(draws=1000, tune=1000, chains=4,
                          target_accept=0.99, random_seed=123)

    idata.to_netcdf(cache_file)
    return idata


def summarize_draws(draws, positive):
    # shortest interval containing 95% of the draws
    values = np.sort(np.asarray(draws).ravel())
    width = round(0.95 * len(values))
    start = np.argmin(values[width:] - values[:-width])
    return {
        "mean": float(values.mean()),
        "hdi": (float(values[start]), float(values[start + width])),
        "pd": float((values > 0).mean() if positive else (values < 0).mean()),
    }


def get_claim2_results(data, refit=False, variants=("original", "revised")):
    rir = (data["protocol"] == "RIR").astype(int)
    vl = (data["muscle"] == "Vastus Lateralis").astype(int)
    pre = data["Pre"] - data.groupby("muscle")["Pre"].transform("mean")
    X = pd.DataFrame({"rir": rir, "vl": vl, "rir_vl": rir * vl, "pre": pre})

    ones = np.ones((len(data), 1))
    limb = pd.MultiIndex.from_frame(data[["id", "limb"]])
    muscle = pd.MultiIndex.from_frame(data[["id", "muscle"]])
    groups = [
        ("id", data["id"], np.column_stack([ones, rir])),
        ("limb", limb, ones),
    ]

    results = {}
    for name in variants:
        model_groups = groups + [("muscle", muscle, ones)] if name == "revised" else groups

        # Add sex to the original model without changing the random effects.
        model_X = X
        if name == "sex_adjusted":
            model_X = X.assign(sex=data["sex"].map({"female": 0, "male": 1}))

        idata = fit_model(
            f"claim2_muscle_{name}",
            data["muscle.thickness"].to_numpy(),
            model_X, model_groups, refit,
        )
        beta = idata.posterior["beta"]
        rf_effect = -beta.sel(predictor="rir")
        vl_effect = rf_effect - beta.sel(predictor="rir_vl")
        quad_effect = (rf_effect + vl_effect) / 2

        results[name] = {
            **summarize_draws(quad_effect, positive=True),
            "p_rf_rir": float((rf_effect < 0).mean()),
            "p_vl_fail": float((vl_effect > 0).mean()),
            "posterior": quad_effect,
            "idata": idata,
        }
    return results


def get_claim4_results(velocity, rep_loss, refit=False):
    # velocity model uses separate indicators for week 4 and week 8
    velocity = velocity.dropna(subset=[
        "percent_diff", "protocol", "time", "sex", "first_set", "sets", "id", "limb"
    ])
    rir = (velocity["protocol"] == "RIR").astype(int)
    week4 = (velocity["time"] == "WEEK 4").astype(int)
    week8 = (velocity["time"] == "WEEK 8").astype(int)
    male = (velocity["sex"] == "male").astype(int)
    X = pd.DataFrame({
        "rir": rir, "week4": week4, "week8": week8,
        "first_set": velocity["first_set"], "sets": velocity["sets"],
        "rir_week4": rir * week4, "rir_week8": rir * week8, "male": male,
    })
    ones = np.ones((len(velocity), 1))
    limb = pd.MultiIndex.from_frame(velocity[["id", "limb"]])
    groups = [
        ("id", velocity["id"], np.column_stack([ones, X.drop(columns="male")])),
        ("limb", limb, ones),
    ]
    velocity_fit = fit_model("claim4_velocity", velocity["percent_diff"].to_numpy(),
                             X, groups, refit)
    beta = velocity_fit.posterior["beta"]

    week4_effect = -beta.sel(predictor="rir") - beta.sel(predictor="rir_week4")
    # set up a Week 1 RIR prediction for a woman
    week1_row = X.mean()
    week1_row[["week4", "week8", "rir_week4", "rir_week8", "male"]] = 0
    week1_row["rir"] = 1

    week1_centered = week1_row - X.mean()
    week1_female = velocity_fit.posterior["intercept"] + (
        beta * week1_centered.to_numpy()
    ).sum(dim="predictor")

   

    week1_male = week1_female + beta.sel(predictor="male")

    male_share = float(male.mean())
    week1_equal = 0.5 * week1_female + 0.5 * week1_male
    week1_prop = (1 - male_share) * week1_female + male_share * week1_male

    

    for label, draws in [
        ("Equal weights", week1_equal),
        ("Proportional weights", week1_prop),
    ]:
        result = summarize_draws(draws * 100, positive=False)
        lo, hi = result["hdi"]
        print(
            f"Week 1 RIR, {label}: mean {result['mean']:.2f}%, "
            f"95% HDI [{lo:.2f}%, {hi:.2f}%], pd (< 0) {result['pd']:.1%}"
        )

    # week 8 RIR at mean first-set velocity and mean sets
    week8_row = X.mean()
    week8_row[["rir", "week4", "week8", "rir_week4", "rir_week8", "male"]] = [1, 0, 1, 0, 1, 0]
    week8_female = velocity_fit.posterior["intercept"] + (
        beta * (week8_row - X.mean()).to_numpy()
    ).sum(dim="predictor")
    week8_equal = week8_female + 0.5 * beta.sel(predictor="male")
    week8_prop = week8_female + male.mean() * beta.sel(predictor="male")

    # center first-set reps within exercise before dropping incomplete rows
    rep_loss = rep_loss.copy()
    rep_loss["reps.first"] -= rep_loss.groupby("exercise")["reps.first"].transform("mean")
    rep_loss = rep_loss.dropna(subset=[
        "rep.loss", "protocol", "time", "exercise", "sex", "reps.first", "sets", "id", "limb"
    ])
    rir = (rep_loss["protocol"] == "RIR").astype(int)
    press = (rep_loss["exercise"] == "Leg Press").astype(int)
    time = rep_loss["time"]
    X = pd.DataFrame({
        "rir": rir, "time": time, "press": press,
        "reps_first": rep_loss["reps.first"], "sets": rep_loss["sets"],
        "rir_time": rir * time, "rir_press": rir * press, "time_press": time * press,
        "rir_time_press": rir * time * press, "male": (rep_loss["sex"] == "male").astype(int),
    })
    ones = np.ones((len(rep_loss), 1))
    limb = pd.MultiIndex.from_frame(rep_loss[["id", "limb"]])
    groups = [
        ("id", rep_loss["id"], np.column_stack([ones, X.drop(columns="male")])),
        ("limb", limb, np.column_stack([ones, X[["time", "press", "reps_first"]]])),
    ]
    rep_fit = fit_model("claim4_rep_loss", rep_loss["rep.loss"].to_numpy(), X, groups, refit)
    beta = rep_fit.posterior["beta"]

    # leg extension is the reference exercise
    rir_slope = beta.sel(predictor="time") + beta.sel(predictor="rir_time")
        #check mixing for the reported contrasts
    contrasts = xr.Dataset({
        "week4_fail_minus_rir": week4_effect * 100,
        "week8_rir_equal": week8_equal * 100,
        "week8_rir_proportional": week8_prop * 100,
        "rir_leg_extension_slope": rir_slope * 100,
    })
    print(az.summary(contrasts, kind="diagnostics", round_to=3))

    return {
        "week4_contrast": summarize_draws(week4_effect * 100, positive=False),
        "week8_rir_equal": summarize_draws(week8_equal * 100, positive=False),
        "week8_rir_prop": summarize_draws(week8_prop * 100, positive=False),
        "rir_leg_extension_slope": summarize_draws(rir_slope * 100, positive=True),
        "velocity_fit": velocity_fit,
        "rep_fit": rep_fit,
        "week1_rir_equal": summarize_draws(week1_equal * 100, positive=False),
        "week1_rir_prop": summarize_draws(week1_prop * 100, positive=False),
    }


def get_claim14_results():
    #solve for the paired effect size at 80% power and alpha 0.05
    #a paired test is a one-sample test of the within-person differences
    results = {}
    for n in (18, 19, 20):
        for sided in ("one-sided", "two-sided"):
            critical = stats.t.ppf(0.95 if sided == "one-sided" else 0.975, n - 1)

            def power(d):
                ncp = d * np.sqrt(n)
                value = stats.nct.sf(critical, n - 1, ncp)
                if sided == "two-sided":
                    value += stats.nct.cdf(-critical, n - 1, ncp)
                return value

            results[(n, sided)] = brentq(lambda d: power(d) - 0.80, 0.05, 1)
    return results


def get_claim15_results(data):
    results = {}
    for exercise in ("Leg Press", "Leg Extension"):
        #average sessions within each participant and protocol, as in the audit
        d = data.loc[data["exercise"] == exercise].dropna(subset=["reps.first"])
        means = d.groupby(["id", "protocol"])["reps.first"].mean().unstack("protocol")
        paired = means[["FAIL", "RIR"]].dropna()
        diff = paired["FAIL"] - paired["RIR"]
        test = stats.ttest_rel(paired["FAIL"], paired["RIR"])

        #also compare only sessions with both limbs present
        #separate protocol means can otherwise cover different sessions
        matched = d.pivot(index=["id", "week", "session"], columns="protocol", values="reps.first")
        matched = matched[["FAIL", "RIR"]].dropna()
        matched_diff = (matched["FAIL"] - matched["RIR"]).groupby("id").mean()
        matched_test = stats.ttest_1samp(matched_diff, 0)

        results[exercise] = {
            "n": len(diff), "mean": float(diff.mean()), "p": float(test.pvalue),
            "matched_n": len(matched_diff), "matched_sessions": len(matched),
            "matched_mean": float(matched_diff.mean()), "matched_p": float(matched_test.pvalue),
        }
    return results

    

