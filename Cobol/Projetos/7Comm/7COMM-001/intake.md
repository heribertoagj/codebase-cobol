# Intake 7COMM-001



## 1. Identificacao

- Consultoria: 7Comm

- ID: 7COMM-001

- Titulo: Correcao chamada GRUP (GRUP0930 → GRUP201C) na malha SLIG

- Data intake: 2026-05-26

- Atualizado: 2026-05-28 (escopo refinado — somente GRUP)



## 2. Negocio

- Problema: Erro na carga do codigo de GRUPO no SLIG apos CNPJ alfanumerico — programas ainda chamam GRUP0930 ou mapeiam CNPJ/filial incorretamente para GRUP201C

- Objetivo: Corrigir chamada e interface GRUP nos 10 PGMs indicados pelo Bradesco

- Iniciativa: CNPJ alfanumerico (modulo GRUP)



## 3. Escopo tecnico — unico vetor



**GRUP0930 → GRUP201C** — copybook GRUPW01S (CNPJ X(11) + filial X(04))



- Centros de custo: SLIG

- Tecnologias: COBOL batch

- **Fora de escopo:** I#CBON01, I#DEVE17/I#DEVEN7, CLIB2000/CLIBB000, JCL, SORT, demais PGMs SLIG



## 4. Contexto operacional

- Gatilho: e-mail Bradesco 28/05/2026 (RES: SLIG - ERRO na carga do codigo do GRUPO)

- PGMs na cadeia SLIG que consultam grupo economico via GRUP

- Referencia de malha: net/LIG1001.txt (contexto apenas)



## 5. Fontes e evidencias

- pgm/: 10 programas GRUP (lista no README)

- cpy/: GRUPW01S (pendente)

- Evidencia: `RES_ SLIG - ERRO na carga do código do GRUPO (novo modulo GRUP201C).msg`



## 6. Regras tecnicas (GRUP)

- Substituir CALL GRUP0930 por GRUP201C

- Substituir COPY I#GRUP34 por GRUPW01S onde aplicavel

- Mapeamento: `'00'` + CNPJ-ST(9) + FILIAL-ST(2:4) + CONTROLE-ST

- Atualizar literais GRUP0930 → GRUP201C em mensagens de erro (se aplicavel)



## 7. Status implementacao (2026-05-28)

- **Corrigidos:** SLIG6100, SLIG6103, SLIG6105, SLIG6216, SLIG6420

- **OK (ja conformes):** SLIG6102, SLIG6210, SLIG6211, SLIG6215, SLIG6226

- **Copybook GRUPW01S:** pendente baixa em cpy/



## 8. Perguntas em aberto

- Padronizar textos `(GRUP0930)` nos SRELCTRL de todos os PGMs?

- Pacotes de subida por PGM (873, 887, 891, 888, 926) — ordem de promocao?

