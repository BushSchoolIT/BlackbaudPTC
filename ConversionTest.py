import pandas as pd
import numpy as np

run_date = "9132022"
semester1 = "fall_2022"
semester2 = "spring_2023"

stu_par = pd.read_excel(
  io = f"./Source/Student_Parent_{run_date}.xlsx"
)
stu_par = stu_par.drop_duplicates(subset=["Student Lastname", "Student Firstname"]).reset_index(drop=True)
stu_par["Group"] = stu_par.groupby(["Parent Lastname", "Parent Firstname"]).ngroup()
stu_par["Parent"] = stu_par["Parent Lastname"] + " " + stu_par["Parent Firstname"] + " " + stu_par["Email"]

ce1 = pd.read_excel(
  io = f"./Source/course_enrollments_{semester1}_{run_date}.xlsx"
)
ce1 = ce1.explode("Student List")
ce1["Student"] = ce1["Student List"].str.strip()

ce2 = pd.read_excel(
  io = f"./Source/course_enrollments_{semester2}_{run_date}.xlsx"
)
ce2 = ce2.explode("Student List")
ce2["Student"] = ce2["Student List"].str.strip()

abg = pd.read_excel("./Source/advisor_by_grade_" + run_date + ".xlsx")
abg.columns = ["Student", "Advisor", "Advisory"]

abg["GradeLevel"] = abg.apply(lambda row: row["Student"] if pd.isna(row["Advisor"]) else None, axis=1)
abg["GradeLevel"] = abg["GradeLevel"].fillna(method="ffill")
abg = abg[(abg["Student"] != "Student") & (~pd.isna(abg["Advisor"]))]

abg[["Student", "GradYear"]] = abg["Student"].str.split(",", expand=True)
abg["GradYear"] = abg["GradYear"].str[1:3]
abg["Student"] = abg["Student"].str.strip()
abg["GradYear"] = 2000 + abg["GradYear"].astype(int)

abg[["LastName", "OtherName"]] = abg["Student"].str.split(",", expand=True)
abg["LastName"] = abg["LastName"].str.strip()
abg["OtherName"] = abg["OtherName"].str.strip()

abg[["FirstName", "PreferredName"]] = abg["OtherName"].str.split("[(]", expand=True)
abg["FirstName"] = abg["FirstName"].str.strip()
abg = abg.drop(columns=["PreferredName"])

abg["JoinName"] = abg["FirstName"] + " " + abg["LastName"]

abg_ce_1 = pd.merge(
    left=abg[["JoinName", "GradeLevel", "GradYear", "Advisor", "Advisory"]],
    right=ce1[["Student", "Teacher", "Section", "Block", "Duration"]],
    left_on="JoinName",
    right_on="Student",
    how="outer"
)
abg_ce_1 = abg_ce_1.rename(columns={"Student": "Student"})

abg_ce_1["GradeLevel"] = pd.Categorical(abg_ce_1["GradeLevel"], categories=["9th Grade", "10th Grade", "11th Grade"])