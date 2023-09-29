from BbApiConnector import BbApiConnector
import os
import pandas as pd
import numpy as np

#path = os.path.join(os.pardir, r'\config\app_secrets.json')
path = r'/home/mlindner/BlackbaudPTC/config/app_secrets.json'

api_conn = BbApiConnector(path)
bb_session = api_conn.get_session()

def api_call(url, params):
    req = bb_session.get(url, params=params)
    output = req.json()
    return output

# Required endpoints:
# https://api.sky.blackbaud.com/school/v1/advisories/sections?level_num={level_num}[&school_year] advisory sections by school level
# https://api.sky.blackbaud.com/school/v1/users?roles={roles}[&first_name][&last_name][&email][&maiden_name][&grad_year][&end_grad_year][&marker] users by role
# https://api.sky.blackbaud.com/school/v1/academics/student/{student_id}/sections academic sections by student
# https://api.sky.blackbaud.com/school/v1/users/{parent_id}/students students by parent

# Role info
# Sample api call:
# req = bb_session.get('https://api.sky.blackbaud.com/school/v1/roles')
# {'id': 13238, 'base_role_id': 16, 'hidden': False, 'name': 'Parent'
# {'id': 13236, 'base_role_id': 14, 'hidden': False, 'name': 'Student'}

# We just need three tables:
# Student Parent ==================
par = pd.DataFrame.from_dict(api_call(
    url = 'https://api.sky.blackbaud.com/school/v1/users',
    params = {
            'roles': '13238',
            })['value'])

par['students'] = np.empty((len(par), 0)).tolist()
for index, parent_id in enumerate(par['id']):        
    relationships = api_call(
        url = 'https://api.sky.blackbaud.com/school/v1/users/' + str(parent_id) + '/relationships',
        params = {
                })
    for person in relationships['value']:
        if person['parental_access'] == True:
            par['students'][index].append(person['first_name'] + ' ' + person['last_name'])
            ','.join(par['students'][index])
            # TODO: strip the brackets

print(par[50:150])


#par_stu = api_call(
#        url = 'https://api.sky.blackbaud.com/school/v1/users/' + str(6857267) + '/relationships',
#        params = {
#})
#for person in par_stu['value']:
#    print(person['first_name'], person['last_name'], person['relationship'], person['parental_access'])


#print(par)
# stu_par = stu_par.groupby(["Student Lastname", "Student Firstname"]).first().reset_index()
# stu_par["Group"] = stu_par.groupby(["Parent Lastname", "Parent Firstname"]).ngroup()
# stu_par["Parent"] = stu_par["Parent Lastname"] + " " + stu_par["Parent Firstname"] + " " + stu_par["Email"]

# Course Enrollment ==================


# Advisor by Grade ==================



# import pandas as pd
# import rpy2.robjects as robjects
# saveRDS = robjects.r['saveRDS']
# saveRDS(pd_dataframe, 'data.rds')
