import extract_msg
path = r'c:\Users\herib\OneDrive\Documents\Workspace\Cobol\Projetos\7Comm\7COMM-001\RES_ SLIG - ERRO na carga do código do GRUPO (novo modulo GRUP201C).msg'
h = extract_msg.Message(path).htmlBody.decode('cp1252', 'replace')
for s in ('GRUPW01S', 'GRUP0930', 'SLIG0930', 'GRUP201C'):
    print(s, s in h)
