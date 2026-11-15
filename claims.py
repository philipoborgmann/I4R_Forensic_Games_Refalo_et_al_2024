from pathlib import Path
import pandas as pd
from claims_models import get_claim2_results, get_claim4_results, get_claim14_results, get_claim15_results
import re
import numpy as np
from openpyxl import load_workbook



project_root = Path(__file__).resolve().parent


def get_data(paths):
    return {path.stem: pd.read_excel(path, sheet_name=None) for path in paths}


def main():
    all_data = get_data(sorted((project_root / "pkg" / "Data").rglob("*.xlsx")))
    volume = all_data["modified.longitudinal.models"]

    #read the article supplied with the data, if no extracted text is available
    paper_path = project_root / "paper" / "paper.txt"
    paper = ""
    if paper_path.exists():
        paper = paper_path.read_text(encoding="utf-8")
    else:
        try:
            import pymupdf
            with pymupdf.open(next((project_root / "pkg").glob("*.pdf"))) as document:
                paper = "\n".join(page.get_text() for page in document)
        except (ImportError, StopIteration):
            print("Article checks need paper/paper.txt or the supplied PDF and pymupdf.")
    paper = " ".join(paper.replace("\u2212", "-").split())

    #######################################################################
    #Claim 1

    # The Volume Load Variable (volume.load) is not, as defined in section 2.4.2 sets x repetitions x load.
    # The ratio between reported volume.load and repetitions * starting laods is under 1 in all 1093 rows and has a median of 0.142.
    # ratio barely changes with repetitions at fixed loads.
    # Interestingly, the Ai audit did not calculate the volume.load variable according to the definition either, but did repetitions x load instead. 
    # If that was to try and asses what measure the authors calculated instead or any other reason can only be sepculated.

    #Load relevant Data
    C1_data = volume["Final Volume Load + Repetitions"]

    #clean NA rows and check number against claims
    C1_data = C1_data.dropna(
        subset=["volume.load", "starting.load", "repetitions", "sets"]
    ).copy()

    #calculate Volume.load as defined
    #for this ratio, use the audit's denominator without sets
    vol_loa_def = C1_data["starting.load"] * C1_data["repetitions"]

    #calcuate ratio: Reported Volume load to repetitions x starting load, check if all ratios < 1 and caluclate median
    C1_data["ratio"] = C1_data["volume.load"] / vol_loa_def

    #check change with repetitions at fixed load
    C1_ratio_loads = C1_data.pivot_table(
        index="repetitions", columns="starting.load", values="ratio", aggfunc="mean"
    )
    C1_cv = 100 * C1_ratio_loads.std() / C1_ratio_loads.mean()

    print("Claim 1")
    print(f"Rows: {len(C1_data)}, ratio < 1: {(C1_data['ratio'] < 1).sum()}")
    print(f"Median ratio: {C1_data['ratio'].median():.3f}")
    print(f"CV across repetition means by load: {C1_cv.min():.1f}% to {C1_cv.max():.1f}%")

    #######################################################################
    #Claim 2

    # Muscle thickness changes correlate between limbs for RF (0.92) and VL (0.83).
    # Adding a participant-by-muscle random effect narrows the quadriceps HDI from [-0.063, 0.058] to [-0.033, 0.029] cm.
    # P(RF favors RIR) goes from 81% to 96%, and P(VL favors FAIL) from 79% to 93%.

    #load relevant data and calculate change
    C2_data = next(iter(all_data["rf_vl_master"].values())).copy()
    C2_data["change"] = C2_data["Post"] - C2_data["Pre"]

    #match FAIL and RIR by participant and muscle
    wide = C2_data.pivot(index=["id", "muscle"], columns="protocol", values="change")
    rf = wide.xs("Rectus Femoris", level="muscle")
    vl = wide.xs("Vastus Lateralis", level="muscle")
    #calculate paired differences for the standard errors
    diff_rf = rf["FAIL"] - rf["RIR"]
    diff_vl = vl["FAIL"] - vl["RIR"]
    quad_diff = (diff_rf + diff_vl) / 2

    print("\nClaim 2")

    #calculate correlations between limbs and between muscles
    print(f"Between-limb correlations: RF {rf['FAIL'].corr(rf['RIR']):.2f}, "
          f"VL {vl['FAIL'].corr(vl['RIR']):.2f}")
    print(f"Between-muscle correlations: FAIL {rf['FAIL'].corr(vl['FAIL']):.2f}, "
          f"RIR {rf['RIR'].corr(vl['RIR']):.2f}")

    #print paired standard errors
    print(f"Paired SEs: RF {diff_rf.sem():.4f}, VL {diff_vl.sem():.4f}, "
          f"quadriceps {quad_diff.sem():.4f}")


    #compare model results with and without the participant-by-muscle term
    C2_results = get_claim2_results(C2_data)
    for name, result in C2_results.items():
        lo, hi = result["hdi"]
        divergences = int(result["idata"].sample_stats["diverging"].sum().item())
        print(f"{name}: mean {result['mean']:.4f} cm, 95% HDI [{lo:.4f}, {hi:.4f}] cm, "
              f"P(RF favors RIR) {result['p_rf_rir']:.1%}, "
              f"P(VL favors FAIL) {result['p_vl_fail']:.1%}, divergences {divergences}")

    #######################################################################
    #Claim 3

    # The Final sheet blanks 24 values where reps increased from first to last set.
    # 22 are on the FAIL limb and 23 are on the leg press.
    # The leg press gap grows from 3.6 to 10.0 percentage points.

    #match rows in the Pre Data Audit and Final sheets
    C3_data_final = volume["Final % Rep Loss"]
    C3_data_pre = volume["% Rep Loss (Pre Data Audit)"]
    keys = ["id", "week", "session", "exercise", "protocol"]
    merged = C3_data_pre[keys + ["rep.change", "rep.loss"]].merge(
        C3_data_final[keys + ["rep.loss"]], on=keys, suffixes=("_pre", "_final")
    )
    #find newly blanked values and check which rows they came from
    blanked = merged.loc[merged["rep.loss_pre"].notna() & merged["rep.loss_final"].isna()]

    print("\nClaim 3")
    print(f"Newly blanked: {len(blanked)}, rep gain: {(blanked['rep.change'] > 0).sum()}, "
          f"FAIL: {(blanked['protocol'] == 'FAIL').sum()}, "
          f"leg press: {(blanked['exercise'] == 'Leg Press').sum()}")
    #compare pooled mean repetition loss on the leg press in each sheet
    for name, data in (("Pre Data Audit", C3_data_pre), ("Final", C3_data_final)):
        means = data.loc[data["exercise"] == "Leg Press"].groupby("protocol")["rep.loss"].mean()
        print(f"{name} leg-press gap (RIR - FAIL): {100 * (means['RIR'] - means['FAIL']):.1f} pp")

    #######################################################################
    #Claim 4

    # The week 4 velocity contrast and RIR leg-extension slope have suspect printed HDIs.
    # For week 8 RIR velocity, the paper uses different sex weights for the HDI and pd.

    print("\nClaim 4")

    #load velocity data and get results from both models
    C4_velocity = all_data["vl_master"]["Velocity Loss"]
    results = get_claim4_results(C4_velocity, C3_data_final)
    #compare refit HDIs and directional probabilities with the paper and audit
    rows = [
        ("week4_contrast", "Week 4 velocity FAIL - RIR", "pp", "<",
         "HDI [-12.4, 1.0], pd 99%", "[-13.0, -1.2]"),
        ("week8_rir_equal", "Week 8 RIR velocity, equal weights", "%", "<",
         "HDI [-12.0, 0.6]", "[-12.3, -0.8]"),
        ("week8_rir_prop", "Week 8 RIR velocity, proportional weights", "%", "<",
         "pd 99%", None),
        ("rir_leg_extension_slope", "RIR leg-extension slope", "pp/session", ">",
         "HDI [0.1, 0.7], pd 94%", "[-0.08, 0.61]"),
    ]
    for key, label, unit, sign, printed, audit_hdi in rows:
        result = results[key]
        lo, hi = result["hdi"]
        print(f"{label}: mean {result['mean']:.2f}, 95% HDI [{lo:.2f}, {hi:.2f}] {unit}, "
              f"pd ({sign} 0) {result['pd']:.1%}")
        print(f"  Paper: {printed}")
        if audit_hdi is not None:
            print(f"  Audit refit HDI {audit_hdi} {unit}")

    #print divergences for both fits
    for name in ("velocity_fit", "rep_fit"):
        divergences = int(results[name].sample_stats["diverging"].sum().item())
        print(f"{name} divergences: {divergences}")
    # Claim 5
    # The paper uses equal weights for the velocity estimate and HDI, but proportional weights for pd.
    # For Week 1 RIR, this pairs an HDI that includes zero with a pd of 98% from a different set of weighted draws.
    print("\nClaim 5")
    # Compare the Week 1 RIR mean, HDI, and pd using the same weights within each row
    for key, label in [
        ("week1_rir_equal", "Equal weights"),
        ("week1_rir_prop", "Proportional weights"),
    ]:
        result = results[key]
        lo, hi = result["hdi"]
        print(
            f"Week 1 RIR, {label}: mean {result['mean']:.2f}%, "
            f"95% HDI [{lo:.2f}%, {hi:.2f}%], pd (< 0) {result['pd']:.1%}"
        )
    # Claim 6
    #Total reps and volume load reported in table 4 are sums not averages.
    #per session averages reproduce 0 of 32 rows for each variable
    #weekly sums reproduce 18 and 25 respectively
    #load lifted row can not be reproduced from deposit
    #figure 2 shows same weekly totals as table 4 while results 3.7 reports per-session model estimates without defining unit 
    print("\nClaim 6")

    # Transcribe the published means and SDs from Table 4
    C6_published = pd.DataFrame(
        [
            # week, protocol, exercise, reps mean, reps SD, volume load mean, volume load SD
            (1, "FAIL", "Leg Press",      46, 10, 546, 289),
            (1, "FAIL", "Leg Extension",  57, 10, 254,  71),
            (1, "RIR",  "Leg Press",      42,  7, 527, 273),
            (1, "RIR",  "Leg Extension",  63, 10, 253,  55),

            (2, "FAIL", "Leg Press",      47,  8, 574, 268),
            (2, "FAIL", "Leg Extension",  59, 11, 244,  48),
            (2, "RIR",  "Leg Press",      46,  9, 555, 254),
            (2, "RIR",  "Leg Extension",  63, 10, 246,  46),

            (3, "FAIL", "Leg Press",      49, 11, 599, 282),
            (3, "FAIL", "Leg Extension",  60, 10, 248,  47),
            (3, "RIR",  "Leg Press",      44,  9, 582, 266),
            (3, "RIR",  "Leg Extension",  64, 10, 252,  46),

            (4, "FAIL", "Leg Press",      46,  8, 592, 266),
            (4, "FAIL", "Leg Extension",  63, 12, 257,  52),
            (4, "RIR",  "Leg Press",      43,  8, 582, 255),
            (4, "RIR",  "Leg Extension",  63, 10, 264,  55),

            (5, "FAIL", "Leg Press",      52, 11, 669, 225),
            (5, "FAIL", "Leg Extension",  63, 15, 303, 110),
            (5, "RIR",  "Leg Press",      49, 11, 668, 227),
            (5, "RIR",  "Leg Extension",  66, 14, 313, 111),

            (6, "FAIL", "Leg Press",      49, 12, 709, 237),
            (6, "FAIL", "Leg Extension",  65, 11, 300,  76),
            (6, "RIR",  "Leg Press",      48, 10, 701, 219),
            (6, "RIR",  "Leg Extension",  65, 11, 307,  78),

            (7, "FAIL", "Leg Press",      51,  8, 740, 274),
            (7, "FAIL", "Leg Extension",  67,  9, 344,  82),
            (7, "RIR",  "Leg Press",      48, 10, 776, 253),
            (7, "RIR",  "Leg Extension",  69, 13, 331,  68),

            (8, "FAIL", "Leg Press",      48, 10, 768, 287),
            (8, "FAIL", "Leg Extension",  68, 13, 330,  76),
            (8, "RIR",  "Leg Press",      44,  9, 770, 277),
            (8, "RIR",  "Leg Extension",  73, 12, 338,  74),
        ],
        columns=[
            "week", "protocol", "exercise",
            "total_reps_mean", "total_reps_sd",
            "volume_load_mean", "volume_load_sd",
        ],
    )

    #calculate weekly averages
    C6_all_data = all_data["modified.longitudinal.models"]
    C6_main = C6_all_data["Final Volume Load + Repetitions"]
    keys = ["week", "protocol", "exercise"]

    # Group the deposited session rows by participant and week
    sessions = C6_main.groupby(keys + ["id"])[["repetitions", "volume.load"]]

    C6_checks = {}
    for label, per_person in [
        ("Session average", sessions.mean()),
        ("Weekly sum", sessions.sum(min_count=1)),
    ]:
        # Calculate mean and SD across participants
        summary = per_person.groupby(level=keys).agg(
            total_reps_mean=("repetitions", "mean"),
            total_reps_sd=("repetitions", "std"),
            volume_load_mean=("volume.load", "mean"),
            volume_load_sd=("volume.load", "std"),
        ).reset_index()

        # Match each calculated row to its published Table 4 row
        check = C6_published.merge(
            summary, on=keys, how="left",
            suffixes=("_published", "_calculated"),
            validate="one_to_one",
        )

        # The published means are rounded to whole units
        for metric in ["total_reps_mean", "volume_load_mean"]:
            error = check[f"{metric}_calculated"] - check[f"{metric}_published"]
            check[f"{metric}_error"] = error
            print(f"{label} | {metric}: {error.abs().le(0.5).sum()}/{len(check)} match")

        C6_checks[label] = check
    
       
    #Claim 7

    #Table 5 reports the share of draws in a fixed direction as pd.
    #The quadriceps 48% means P(FAIL > RIR), not P(a difference exists).

    print("\nClaim 7")

    # Check which side of zero the authors counted
    analysis_path = next((project_root / "pkg").rglob("Refalo et al Data Analysis.qmd"))
    analysis = analysis_path.read_text(encoding="utf-8-sig")

    for label, heading, sign in [
        ("Quadriceps contrast", "# Contrast for protocol (>o)", ">"),
        ("RF contrast", "# Contrast for protocol by RF (<0)", "<"),
        ("Leg press slope contrast", "# By protocol and exercise (slopes for leg press <0)", "<"),
    ]:
        block = analysis.partition(heading)[2].split("\n# ", 1)[0]
        print(f"{label}, code counts draws {sign} 0: {f'mean(.value{sign}0' in block}")

    # Count both directions using the original FAIL - RIR draws from Claim 2
    quad_draws = C2_results["original"]["posterior"]
    p_fail = float((quad_draws > 0).mean())
    p_rir = float((quad_draws < 0).mean())

    print(f"Quadriceps P(FAIL > RIR): {p_fail:.1%} (paper: 48%)")
    print(f"Quadriceps P(RIR > FAIL): {p_rir:.1%}")
    print(f"Quadriceps probability of direction: {max(p_fail, p_rir):.1%}")

    # Check the printed values and Discussion wording if paper text is available
    if paper:

        print("Table 5 prints quadriceps pd of 48%:",
              bool(re.search(r"Quadriceps Thickness -0\.001 cm -0\.063 to 0\.058 48%", paper)))
        print("Table 5 prints leg press slope pd of 49%:",
              bool(re.search(r"Leg Press 0 repetitions -0\.17 to 0\.19 49%", paper)))
        print("Discussion uses the 48% wording:",
              "with a 48% probability (pd > TE = 3%) that any poten" in paper)
    else:
        print("Check Table 5 and the Discussion manually (paper/paper.txt not found).")

       
    #Claim 8

    # Check which data reads point to missing files or sheets.
    # Test the folder-name fixes without changing the deposited scripts.
    print("\nClaim 8")

    pkg = project_root / "pkg"
    data_dir = pkg / "Data"
    read_call = re.compile(r'read_(?:excel|xlsx)\(\s*"([^"]+)"(?:\s*,\s*(\d+))?')

    total_calls = 0
    bad_paths = 0
    missing_files = set()
    missing_sheets = []
    bad_sheets_as_written = 0

    # Read the calls from all five QMD files
    for script in sorted(pkg.rglob("*.qmd")):
        for path, sheet in read_call.findall(script.read_text(encoding="utf-8-sig")):
            total_calls += 1
            path_exists = (data_dir / path).is_file()
            bad_paths += not path_exists

            # Fix the two folder names and check again
            fixed_path = path.replace("Muscle_Thickness/", "Muscle Thickness/")
            fixed_path = fixed_path.replace("Velocity_Loss/", "Velocity Loss/")
            workbook = data_dir / fixed_path

            if not workbook.is_file():
                missing_files.add(fixed_path)
                continue

            # R uses the first sheet when no sheet number is given
            sheet_no = int(sheet or 1)
            if sheet_no > len(all_data[workbook.stem]):
                missing_sheets.append(f"{script.name}: {fixed_path}, sheet {sheet_no}")
                bad_sheets_as_written += path_exists

    print(f"Paths that fail as written: {bad_paths}/{total_calls}")
    print(f"Calls that fail as written, including sheets: "
          f"{bad_paths + bad_sheets_as_written}/{total_calls}")

    print(f"Missing workbooks after folder fixes: {len(missing_files)}")
    for path in sorted(missing_files):
        print(f"  {path}")

    print(f"Calls to missing sheets after folder fixes: {len(missing_sheets)}")
    for item in missing_sheets:
        print(f"  {item}")

    # Look for documentation and package-version records
    docs = [
        path.name for path in pkg.rglob("*")
        if path.is_file()
        and re.search(r"readme|licen[cs]e|codebook|dictionary|session[ _-]?info|renv\.lock",
                      path.name, re.I)
    ]
    print("Documentation or version files found:", docs)

       
    #Claim 9

    # The muscle-thickness model adjusts for baseline thickness (Pre), but not sex.
    # The other four primary models include sex as a population-level effect.
    # Four of the five longer diagnostic runs change the random-effects formula.

    print("\nClaim 9")

    # Read the original model formulas from the Analysis script.
    pkg = project_root / "pkg"
    analysis = next(pkg.rglob("Refalo et al Data Analysis.qmd")).read_text(encoding="utf-8-sig")
    diagnostics = next(pkg.rglob("Refalo et al Diagnostics.qmd")).read_text(encoding="utf-8-sig")

    formulas = dict(re.findall(r"(?m)^(\w+)_model\s*<-\s*brm\(([^,\n]+),", analysis))

    # Check the population-level terms, before the random-effects brackets.
    for name, formula in formulas.items():
        fixed = formula.split("~", 1)[1].split("(", 1)[0]
        print(f"{name} includes sex: {bool(re.search(r'\bsex\b', fixed))}")
        if name == "MT":
            print(f"MT adjusts for baseline (Pre): {bool(re.search(r'\bPre\b', fixed))}")

    # Compare only the 16,000-iteration runs and ignore spacing.
    reruns = {}
    for chunk in diagnostics.split("```{r}")[1:]:
        chunk = chunk.split("```", 1)[0]
        match = re.search(r"(?m)^(\w+)_model_double_iter\s*<-\s*brm\(([^,\n]+),", chunk)
        if match and re.search(r"\biter\s*=\s*16000\b", chunk):
            reruns[match[1]] = match[2]

    changed = [
        name for name, formula in reruns.items()
        if "".join(formula.split()) != "".join(formulas[name].split())
    ]
    print(f"16,000-iteration runs with changed formulas: {len(changed)}/{len(reruns)}")
    print("Models changed:", ", ".join(changed))

    # Compare the same quadriceps contrast before and after adding sex.
    original = C2_results["original"]["mean"]
    adjusted = get_claim2_results(C2_data, variants=("sex_adjusted",))["sex_adjusted"]["mean"]

    print(f"Quadriceps FAIL - RIR without sex: {original:.4f} cm")
    print(f"Quadriceps FAIL - RIR with sex:    {adjusted:.4f} cm")
    print(f"Shift after adding sex:           {adjusted - original:+.4f} cm")

    #Claim 10

    # The deposit includes six sex-differences models although the paper says these were not analysed.
    # The code uses capitalised sex labels where the workbooks use lowercase labels.
    # Four of 899 rep.change values disagree with final minus first repetitions.

    print("\nClaim 10")

    #read both sex-differences scripts and count the model calls
    sex_analysis = next(pkg.rglob("Sex Diff_Data Analysis_Final.qmd")).read_text(encoding="utf-8-sig")
    sex_prep = next(pkg.rglob("Sex Diff_Data Preparation_Final.qmd")).read_text(encoding="utf-8-sig")
    sex_models = re.findall(r"(?m)^(\w+)\s*<-\s*brm\(([^,\n]+),", sex_analysis)
    print(f"Sex-differences models: {len(sex_models)} (audit: 6)")
    for name, formula in sex_models:
        print(f"  {name}: {formula.strip()}")

    C10_mt = all_data["mt_master_avg"]["MT"]
    labels = pd.concat([d["sex"] for d in [C3_data_final, C6_main, C4_velocity, C2_data, C10_mt]])
    print("Workbook sex labels:", sorted(labels.dropna().unique()))
    print("Code uses capitalised sex labels:",
          bool(re.search(r'sexMale|"Female"|"Male"', sex_analysis)))
    #R factor levels are case-sensitive, so these labels become missing
    recoded = pd.Categorical(labels, categories=["Female", "Male"])
    print(f"Labels lost with Female/Male levels: {pd.isna(recoded).sum()}/{len(labels)}")

    #check the file and sheet failures in these two scripts separately
    for path, sheet in read_call.findall(sex_prep + "\n" + sex_analysis):
        fixed_path = path.replace("Muscle_Thickness/", "Muscle Thickness/")
        fixed_path = fixed_path.replace("Velocity_Loss/", "Velocity Loss/")
        workbook = data_dir / fixed_path
        if not workbook.is_file():
            print(f"Missing after folder fixes: {fixed_path}")
        elif int(sheet or 1) > len(all_data[workbook.stem]):
            print(f"Missing sheet after folder fixes: {fixed_path}, sheet {sheet}")

    C10_data = C3_data_final.dropna(subset=["reps.first", "reps.final", "rep.change"]).copy()
    C10_data["calculated"] = C10_data["reps.final"] - C10_data["reps.first"]
    wrong = C10_data.loc[(C10_data["rep.change"] - C10_data["calculated"]).abs() > 1e-9]
    print(f"rep.change mismatches: {len(wrong)}/{len(C10_data)} (audit: 4/899)")
    print(wrong[["id", "week", "session", "exercise", "protocol", "rep.change", "calculated"]].to_string(index=False))
    if paper:
        print("Paper says sex differences were not specifically analysed:",
              "specifically analyse sex differences" in paper)
    #having these scripts does not tell us when, or whether, they were run
    print("The scripts establish that this analysis was prepared, not that it was completed.")


    #Claim 11

    # The RIR Post SD refers to the change column instead of the Post column.
    # One participant has a different sex label in mt_diff_analysis_avg.
    # Neither of these cells is used in the five primary model fits.

    print("\nClaim 11")

    C11_mt = all_data["mt_master_avg"]["MT"]
    C11_means = all_data["mt_master_avg"]["MT Means"]
    C11_diff = all_data["mt_diff_analysis_avg"]["MT Diff"]
    rir = C11_mt.loc[C11_mt["protocol"] == "RIR"]
    cached = C11_means.loc[(C11_means["protocol"] == "RIR") & (C11_means["time"] == "Post"), "sd"].item()

    #read the formula itself, not only the cached value
    workbook = load_workbook(data_dir / "Muscle Thickness/mt_master_avg.xlsx", data_only=False)
    means_sheet = workbook["MT Means"]
    for row in means_sheet.iter_rows(min_row=2):
        if row[0].value == "RIR" and row[1].value == "Post":
            print(f"MT Means!{row[3].coordinate}: {row[3].value}")
    print("MT column E:", workbook["MT"]["E1"].value, "| column F:", workbook["MT"]["F1"].value)
    workbook.close()
    print(f"Cached SD: {cached:.3f}, change SD: {rir['diff'].std():.3f}, Post SD: {rir['Post'].std():.3f} cm")

    #match by id before comparing sex labels
    master = C11_mt[["id", "sex"]].drop_duplicates()
    diff = C11_diff[["id", "sex"]].drop_duplicates()
    compared = master.merge(diff, on="id", suffixes=("_master", "_diff"), validate="one_to_one")
    changed = compared.loc[compared["sex_master"] != compared["sex_diff"]]
    print("Master counts:", master["sex"].value_counts().to_dict())
    print("Difference-file counts:", diff["sex"].value_counts().to_dict())
    print(f"Participants relabelled: {len(changed)} (audit: 1)")

    #check the disputed label against each of the other six workbooks
    for name, sheets in all_data.items():
        if name == "mt_diff_analysis_avg":
            continue
        records = pd.concat([d[["id", "sex"]] for d in sheets.values() if {"id", "sex"}.issubset(d.columns)])
        records = records.drop_duplicates()
        check = records.merge(changed[["id", "sex_master"]], on="id", validate="many_to_one")
        print(f"{name}: matching master labels {check['sex'].eq(check['sex_master']).sum()}/{len(changed)}")

    #print the data objects used by the primary models to trace the affected cells
    for name, data in re.findall(r"(?m)^(\w+)_model\s*<-\s*brm\([^,]+,\s*data\s*=\s*(\w+)", analysis):
        print(f"Primary model {name} uses {data}")
    print("The summary SD and MT Diff sex label are not inputs to these primary model fits.")

    #######################################################################
    #Claim 12

    # The models use 102 velocity rows, 874 repetition-loss rows and 1093 volume rows.
    # Leg press is missing in attended sessions, including 13 sessions with both limbs missing.
    # 14 out of 16 sessions is 87.5%, not 90%.

    print("\nClaim 12")

    #use all variables in each formula, including group labels
    for label, data, columns, expected in [
        ("Velocity", C4_velocity, ["percent_diff", "protocol", "time", "sex", "first_set", "sets", "id", "limb"], 102),
        ("Repetition loss", C3_data_final, ["rep.loss", "protocol", "time", "exercise", "sex", "reps.first", "sets", "id", "limb"], 874),
        ("Volume load", C6_main, ["volume.load", "protocol", "time", "exercise", "sex", "sets", "id", "limb"], 1093),
        ("Repetition volume", C6_main, ["repetitions", "protocol", "time", "exercise", "sex", "sets", "id", "limb"], 1093),
    ]:
        used = data[columns].notna().all(axis=1)
        print(f"{label}: {used.sum()}/{len(data)} complete rows (audit: {expected})")

    #a set count on either exercise and limb counts as an attended session
    keys = ["id", "week", "session"]
    C12_data = C6_main.assign(has_sets=C6_main["sets"].notna())
    attended = C12_data.groupby(keys)["has_sets"].any().rename("attended")
    lp = C12_data.loc[C12_data["exercise"] == "Leg Press"].merge(attended, on=keys, validate="many_to_one")
    lp = lp.loc[lp["attended"]]
    missing = lp.loc[~lp["has_sets"]]
    both = lp.groupby(keys)["has_sets"].agg(["size", "sum"])
    both = both.loc[(both["size"] == 2) & (both["sum"] == 0)]
    print(f"Missing leg-press rows in attended sessions: {len(missing)}, participants: {missing['id'].nunique()} (audit: 29, 4)")
    print(f"Both limbs missing: {len(both)} sessions, participants: {both.index.get_level_values('id').nunique()} (audit: 13, 3)")
    print(f"14/16 sessions: {100 * 14 / 16:.1f}%, sessions needed for at least 90%: {int(np.ceil(0.90 * 16))}")
    if paper:
        print("Paper gives 14 out of 16:", "14 out of 16 RT sessions" in paper)
        print("Paper describes two unable to perform leg press:",
              "two of the eight unable to perform the leg press" in paper)
    #a missing row does not itself tell us why the exercise was missing
    print("The deposit does not establish whether these gaps were caused by discomfort.")

    #######################################################################
    #Claim 13

    # Limb 1 is always FAIL and limb 2 always RIR in the four primary sheets.
    # These codes do not give anatomical side or dominance.
    # id:limb is the same grouping as id:protocol, not the same effect as fixed protocol.

    print("\nClaim 13")

    C13_data = {"RF and VL": C2_data, "Velocity Loss": C4_velocity,
                "Final % Rep Loss": C3_data_final, "Final Volume Load + Repetitions": C6_main}
    for name, data in C13_data.items():
        print(name)
        print(pd.crosstab(data["limb"], data["protocol"]).to_string())
        expected = data["limb"].map({1: "FAIL", 2: "RIR"})
        print(f"Mismatches or missing codes: {(expected.isna() | data['protocol'].ne(expected)).sum()}/{len(data)}")
        grouped = data[["id", "limb", "protocol"]].drop_duplicates()
        same_groups = not grouped.duplicated(["id", "limb"]).any() and not grouped.duplicated(["id", "protocol"]).any()
        print("id:limb and id:protocol identify the same groups:", same_groups)

    #check all deposited headers for a side or dominance variable
    side_columns = [(name, sheet, str(col)) for name, sheets in all_data.items()
                    for sheet, data in sheets.items() for col in data.columns
                    if re.search(r"left|right|domin|side|anatom", str(col), re.I)]
    print("Side or dominance columns:", side_columns)
    print("Four primary sheets in three workbooks. Randomisation cannot be reconstructed from these limb codes.")
    print("The random effect still describes participant-protocol variation; it does not duplicate the fixed protocol coefficient.")

    #######################################################################
    #Claim 14

    # Changes in group means reproduce the printed percentages, not the mean of individual changes.
    # Figure 5 gives the opposite subtraction order to the deposited velocity changes.
    # d = 0.59 can be reproduced for n = 19, one-sided, at alpha 0.05 and 80% power.
    # This is one possible reconstruction, not evidence of which test was actually used.

    print("\nClaim 14")

    for protocol in ("FAIL", "RIR"):
        data = C11_mt.loc[C11_mt["protocol"] == protocol]
        group = 100 * (data["Post"].mean() / data["Pre"].mean() - 1)
        individual = 100 * (data["Post"] / data["Pre"] - 1).mean()
        print(f"{protocol}: change in group means {group:.2f}%, mean individual change {individual:.2f}%")

    #compare the stored velocity change with both subtraction orders
    data = C4_velocity.dropna(subset=["first_set", "final_set", "percent_diff"])
    calculated = (data["final_set"] - data["first_set"]) / data["first_set"]
    matches = np.isclose(data["percent_diff"], calculated, atol=1e-9, rtol=0)
    opposite = np.isclose(data["percent_diff"], -calculated, atol=1e-9, rtol=0)
    print(f"Velocity matches (final - first)/first: {matches.sum()}/{len(data)}")
    print(f"Velocity matches (first - final)/first: {opposite.sum()}/{len(data)}")
    if paper:
        caption = paper.partition("Figure 5.")[2].split("JOURNAL OF SPORTS SCIENCES", 1)[0]
        formula = re.search(r"Percentage change in lifting velocity calculated as: (.*?\.)", caption)
        print("Figure 5 formula:", formula[1] if formula else "not located in extracted text")

    #compare the possible sample sizes instead of assuming which n was intended
    for (n, sided), effect in get_claim14_results().items():
        print(f"Paired test, n = {n}, {sided}, alpha 0.05, power 80%: d = {effect:.3f}")
    print(f"18 retained with 20% dropout requires {18 / 0.8:.1f}, rounded up {int(np.ceil(18 / 0.8))} recruits")
    print(f"Adding 20% to 18 instead gives {18 * 1.2:.1f}, not 20")
    print(f"20 recruits with 20% dropout leaves {20 * 0.8:.0f}")
    #the paper's Methods and captions do not specify the HDI coverage or power-test settings
    #absence of these details cannot be established by matching the d value alone
    if paper:
        #95% also occurs as pd, and 7.95% is a muscle-thickness change
        #check the statistical Methods rather than matching any occurrence of 95%
        methods = paper.partition("2.5. Statistical analysis")[2].split("3. Results", 1)[0]
        print("Statistical Methods located:", bool(methods))
        print("Percentages mentioned in statistical Methods:",
              re.findall(r"(?<![\d.])\d+(?:\.\d+)?\s*%", methods))
        #the 0% and 100% in this section describe the range of pd, not HDI coverage
        interval_sentences = [sentence.strip() for sentence in re.split(r"(?<=[.!?])\s+", methods)
                              if re.search(r"credible intervals|\bHDI\b", sentence)]
        for sentence in interval_sentences:
            print("Interval description:", sentence)
        print("Paper states d = 0.59:", bool(re.search(r"d\s*=\s*0\.59", paper)))

    #######################################################################
    #Claim 15

    # First-set gaps average 0.89 repetitions on leg press and 0.22 on leg extension.
    # The audit averages each protocol within participant before the paired t test.
    # A first-set gap between limbs is not a direct measurement of achieved RIR.

    print("\nClaim 15")

    for exercise, result in get_claim15_results(C3_data_final).items():
        print(f"{exercise}: FAIL - RIR {result['mean']:.2f} reps, paired p = {result['p']:.4f}, n = {result['n']}")
        print(f"  Matched sessions only: {result['matched_mean']:.2f} reps, p = {result['matched_p']:.4f}, "
              f"n = {result['matched_n']}, paired sessions = {result['matched_sessions']}")

    #RIR can appear as a protocol or summary column without measuring achieved RIR
    rir_columns = [(name, sheet, str(col)) for name, sheets in all_data.items()
                   for sheet, data in sheets.items() for col in data.columns
                   if re.search(r"rir|reserve|accuracy", str(col), re.I)]
    print("Headers referring to RIR, reserve or accuracy:", rir_columns)
    print("These are protocol summaries; no achieved training RIR variable is deposited.")
    if paper:
        print("Paper describes 1- and 3-RIR accuracy targets:", "1- and 3-RIR targets" in paper)
        print("Training used leg press 2-RIR and leg extension 1-RIR:",
              "press to 2-RIR and leg extension to 1-RIR" in paper)
    print("The first-set gaps cannot establish how close each limb actually came to failure.")

    #######################################################################
    #Claim 16

    # Seven workbooks contain participant records for 18 participants.
    # 12 of 16 sheets contain an id column, with no direct identifier among the headers.
    # Workbook creator fields are document metadata, not participant measurements.

    print("\nClaim 16")

    sheets_total = 0
    sheets_with_id = 0
    personal_headers = []
    creators = 0
    modifiers = 0
    for path in sorted(data_dir.rglob("*.xlsx")):
        sheets = all_data[path.stem]
        ids = []
        for sheet, data in sheets.items():
            sheets_total += 1
            if "id" in data.columns:
                sheets_with_id += 1
                ids.append(data["id"].dropna())
        ids = pd.concat(ids)
        integer_ids = pd.to_numeric(ids, errors="coerce")
        print(f"{path.name}: {ids.nunique()} participants, integer ids: "
              f"{bool(integer_ids.notna().all() and integer_ids.mod(1).eq(0).all())}")
        #count populated properties without printing the person names
        workbook = load_workbook(path, read_only=True, data_only=True)
        #read original headers because pandas calls empty columns Unnamed
        #those generated names would falsely match the audit's name pattern
        for sheet in workbook:
            headers = next(sheet.iter_rows(min_row=1, max_row=1, values_only=True))
            personal_headers.extend((path.name, sheet.title, str(col)) for col in headers if col is not None
                                    and re.search(r"name|mail|birth|dob|date|age$|^age|address|phone|comment|note", str(col), re.I))
        creators += bool(workbook.properties.creator and workbook.properties.creator.strip())
        modifiers += bool(workbook.properties.lastModifiedBy and workbook.properties.lastModifiedBy.strip())
        workbook.close()

    print(f"Sheets with id: {sheets_with_id}/{sheets_total} (audit: 12/16)")
    print("Master participant counts:", master["sex"].value_counts().to_dict())
    print(f"Personal-data header matches: {len(personal_headers)} (audit: 0)")
    print(f"Workbooks with creator: {creators}, last-modified-by: {modifiers} (audit: 7 each)")
    if paper:
        print("Article states consent to publication:",
              "All participants provided consent for their data to be published." in paper)
        print("Anonymisation wording in article:", bool(re.search(r"anonym|pseudonym", paper, re.I)))
    #these checks do not establish anonymity or the scope of the original consent
    print("No direct identifier was found in the deposited column headers. This is not a proof of anonymity.")
    
   







if __name__ == "__main__":
    main()
