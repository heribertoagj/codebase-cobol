# Intake 7COMM-003

## 1. Identificacao

- Consultoria: 7Comm
- ID: 7COMM-003
- Titulo: CNPJ SACADO zerado — SLIGB010 (CLI_CHEQ_CONSL) / DEPDC_BANDA_MAGNT
- Data da reuniao: N/A — demanda recebida por e-mail em 2026-06-19
- Participantes: Edinelson Pacci (Bradesco); Luciana Cristina Sato, Anderson Garcia (7Comm); Rodolfo Silva, Aline Coutinho, Julyo Maia, Bruno Assuncao (Bradesco — Cc)
- Responsavel consultoria: Luciana Cristina Sato
- Responsavel Bradesco: Edinelson Pacci (4255/DS — Sustentacao)
- Referencia externa: e-mail `ENC_ SLIG CNPJ Alfa Cheques - Tabela SLIGB010 - CLI_CHEQ_CONSL - CNPJ SACADO zerado .msg`

## 2. Negocio

- Problema ou sintoma reportado: Tabela **SLIGB010** (`CLI_CHEQ_CONSL`) com **todos os registros** com campos de **CNPJ SACADO zerados** na tabela DB2 `DB2PRD.DEPDC_BANDA_MAGNT`. Evidencia: `CCGC_CPF_SACDO_ST`, `CFLIAL_CGC_SACDO`, `CCTRL_CGC_SACDO` = zeros; `CINDCD_PSSOA_SACDO` preenchido (F/J).
- Objetivo da alteracao: Investigar e corrigir os **jobs/programas** que montam a **carga diaria** da tabela SLIGB010, garantindo populacao correta do CNPJ do sacado (contexto **CNPJ alfanumerico** — cheques SLIG).
- Regra de negocio conhecida: Campos de identificacao do sacado na banda magnetica / consolidado de cheques devem refletir o CNPJ/CPF do sacado; nao podem permanecer zerados quando ha indicador de pessoa (F/J).
- Iniciativa regulatoria: **CNPJ alfanumerico** (mesma linha 7COMM-001 / SLIG)
- Prazo / criticidade: **Critica** (producao — todos os registros afetados)

## 3. Escopo tecnico

- Centros de custo envolvidos: **SLIG**
- Tecnologias: COBOL **BATCH**, **JCL**, **DB2** (carga tabela), arquivos sequenciais (MVTOCHDE, EMITCHEQ, etc.)
- Programas citados (cadeia malha cheques — steps JCL):
  - SLIGC184, SLIGC182, SLIGC105 (inicio cadeia — DEPBANDA / CONSCHEC)
  - SLIG1218, SLIG1216, SLIG1215, SLIG1210, SLIG1225 (processamento cheques)
  - SLIG7120, SLIG7110, SLIG0225 (MVTOCHDE / saida / interface CHEC)
- Hipotese step JCL → PGM COBOL: SLIG1210→SLIG6210, SLIG1215→SLIG6215, SLIG1216→SLIG6216 (padrao SLIG; **confirmar no MF**)
- Jobs / JCL citados: steps acima; datasets MX.SLIG.*
- Datasets / arquivos citados:
  - MX.SLIG.JC182S03.DEPBANDA
  - MX.SLIG.JC105S03.CONSCHEC (vazio em TU)
  - MX.SLIG.EMITCHEQ, MX.SLIG.PREVCHEQ, MX.SLIG.EMITCH01, MX.SLIG.DECECHEQ
  - MX.SLIG.D0PRVMZ.FIMPERCH
  - MX.SLIG.PRV.MVTOCHDE (contem I#SLIG27)
  - MX.SLIG.MVTOCHDE.SAIDA
  - MX.CHEC.SLIG.MVTOCHDE
- Tabelas DB2 citadas: `DB2PRD.DEPDC_BANDA_MAGNT` (alias operacional **SLIGB010** / `CLI_CHEQ_CONSL`)
- Copybooks / layouts citados: **I#SLIG27** (arquivo MVTOCHDE — 300 bytes); possivel DCLGEN SLIGB010
- Ponto de entrada do problema: carga diaria SLIGB010 via **SLIGC184** (DB2 LOAD) ← **SLIGC182** (**SLIG6801** gera DEPBANDA) ← **SLIGC105** (**SLIG6224** gera CONSCHEC) ← EMITCHEQ

## 4. Contexto operacional

- Fluxo batch envolvido: malha de processamento de **cheques SLIG** (DEPBANDA → CONSCHEC → EMITCHEQ → … → MVTOCHDE → MVTOCHDE.SAIDA → carga DB2 / interface CHEC)
- O que funciona vs. o que esta quebrado: `CINDCD_PSSOA_SACDO` preenchido; campos **CNPJ sacado** (`CCGC_CPF_SACDO_ST`, filial, controle) **zerados** em todos os registros; valores VCHEQ_* tambem zerados nos prints
- Alteracoes ja feitas por outra equipe: **7COMM-001** — correcao GRUP0930→GRUP201C e CNPJ alfa em PGMs SLIG610x/621x (escopo distinto — nao inclui carga SLIGB010)
- Dependencias com outras demandas: **7COMM-001** (mesmo centro SLIG, CNPJ alfa); possivel impacto em SLIG6210/6215/6216 (ja no acervo 7COMM-001)

## 5. Fontes e evidencias

- Fontes ja baixadas localmente: **nao** na pasta 7COMM-003
- Referencia acervo 7COMM-001: SLIG6210, SLIG6215, SLIG6216 (malha cheques — **somente analise** cruzada)
- Referencia LIBS: `i#slig27.cpy`, `i#sligia.cpy` (menciona SLIGB001-CLI_CHEQ_CONSL)
- Fontes P1 a baixar: ver README `## Fontes pendentes baixa`
- Anexos: `anexos-email/image001.png`, `image002.png`; `.msg` original na raiz da demanda

## 6. Regras tecnicas ditas na reuniao

- Padroes: investigar origem do zeramento antes de corrigir; alinhar com tratamento CNPJ alfa (`PIC X` / campos `-ST`)
- Fora de escopo (preliminar): alteracoes GRUP/I#CBON01 da 7COMM-001 salvo dependencia confirmada
- Restricoes: carga diaria — atencao a janela batch SLIG

## 7. Testes e riscos

- Cenarios: SELECT em DEPDC_BANDA_MAGNT — CNPJ sacado preenchido apos carga; regressao malha cheques; CNPJ numerico e alfanumerico
- Riscos: programa de carga nao mapeado; I#SLIG27 sem campos sacado no layout local (somente emitente); arquivo CONSCHEC vazio em TU dificulta rastreio
- Regressao: malha batch cheques + interface MX.CHEC.SLIG.MVTOCHDE

## 8. Notas livres

- Luciana (7Comm): "Nao encontrei programas que carregam essa base."
- Copybook I#SLIG27 (LIBS) descreve cheque diario/decenal — campos **emitente**; campos **sacado** podem estar em outro layout ou versao mais recente do copybook no MF (#000876).
- Steps SLIG7120/SLIG7110 candidatos a proximidade da carga DB2 / saida final.

## Perguntas em aberto

1. ~~Qual PGM COBOL executa INSERT/LOAD em DEPDC_BANDA_MAGNT?~~ **Respondido:** LOAD utilitario SLIG#010 no job SLIGC184; gerador do arquivo = **SLIG6801**.
2. ~~Mapeamento step JCL → PGM?~~ **Parcial:** ver README `## Malha batch`.
3. ~~DCLGEN SLIGB010?~~ **Ok** em LIBS.
4. Como SLIG6801 deriva **sacado** a partir de CONSCHEC (I#SLIG28)? Cliente = sacado?
5. SLIG4760 impacta CNPJ alfa ao substituir F/J por 99991/99992?
6. Relacao formal com ticket/mudanca Bradesco alem do e-mail?
