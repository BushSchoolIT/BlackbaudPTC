from BbApiConnector import BbApiConnector
import os

#path = os.path.join(os.pardir, r'\config\app_secrets.json')
path = r'/home/mlindner/BlackbaudPTC/config/app_secrets.json'

api_conn = BbApiConnector(path)
bb_session = api_conn.get_session()

# required endpoints:
# https://api.sky.blackbaud.com/school/v1/advisories/sections?level_num={level_num}[&school_year] advisory sections by school level
# https://api.sky.blackbaud.com/school/v1/users?roles={roles}[&first_name][&last_name][&email][&maiden_name][&grad_year][&end_grad_year][&marker] users by role
# https://api.sky.blackbaud.com/school/v1/academics/student/{student_id}/sections academic sections by student

# sample api call
# params = {
#     'level_id': 779,
#     'day': '3/14/2022',
#     'offering_type': 1
#  }
# req = bb_session.get("https://api.sky.blackbaud.com/school/v1/attendance", params=params)

# req = bb_session.get('https://api.sky.blackbaud.com/school/v1/roles')
# {'id': 13236, 'base_role_id': 14, 'hidden': False, 'name': 'Student'}

params = {
    'roles': '13246',
    'grad_year': '2024',
    'end_grad_year': '2027'
 }
req = bb_session.get('https://api.sky.blackbaud.com/school/v1/users', params=params)


print(req.url)
print(req.json())

print(req.json()['value'])
# output = req.json()['value']

# for i in range(len(output)):
#     print(output[i]['student_user_id'])


