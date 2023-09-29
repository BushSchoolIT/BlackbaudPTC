from BbApiConnector import BbApiConnector

path = r'/home/mlindner/BlackbaudPTC/config/app_secrets.json'
api_conn = BbApiConnector(path)
bb_session = api_conn.get_session()

req = bb_session.get('https://api.sky.blackbaud.com/school/v1/roles')
print(req.encoding)
print(req.json())


