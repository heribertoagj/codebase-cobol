
import extract_msg
from bs4 import BeautifulSoup
path = r'c:\Users\herib\OneDrive\Documents\Workspace\Cobol\Projetos\7Comm\7COMM-001\RES_ SLIG - ERRO na carga do código do GRUPO (novo modulo GRUP201C).msg'
msg = extract_msg.Message(path)
html = msg.htmlBody.decode('cp1252')
soup = BeautifulSoup(html, 'lxml')
# pre and code blocks
for pre in soup.find_all(['pre', 'code']):
    print('PRE:', pre.get_text())
# list items full text
for ol in soup.find_all('ol'):
    print('--- OL ---')
    for li in ol.find_all('li', recursive=False):
        print(li.get_text(' ', strip=True))
msg.close()
