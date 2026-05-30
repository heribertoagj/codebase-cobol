# AGROLEG-1482 - SICOR 5.12 - Alteração do Contrato (COR0001)

## Status workflow
- Fase atual: Compilado Changeman — aguardando homologacao
- Ultimo comando: Ticket promocao TICKET-85280 + pacotes RUCA000752 / RUCA000753
- Proximo passo: Testes funcionais RUCA op.33 + validacao payload COR0001 vs XSD 5.12

## Objetivo
- Eu como PO
- Quero alterar o layout da mensageria COR0001, no sistema RUCA opção 33, para excluir e incluir campos conforme as regras divulgadas no catálogo Sicor 5.12.
- Na formalização do contrato, sistema RUCA, alterar o programa para preencher corretamente os campos criados, aplicando as novas regras do SICOR, e excluir envio das informações da certificação orgânica e rastreabilidade, mantendo as informações em tela e base de dados.
- Para garantir que a alteração de contrato no RUCA (opção 33 / Ajustes SICOR / subopção 01) gere a COR0001 conforme layout SICOR BACEN 5.12.

## Restricao de escopo (RUCA-only)

Esta historia altera **somente fontes do centro de custo RUCA**. Nao ha desenvolvimento em RUEC, CRUR, RCOR ou TMNF.

| Incluido | Excluido (outras historias / so teste) |
|----------|----------------------------------------|
| `RUCA8810`, `I#RUCA66` | `RUEC8810`, `I#RUEC66`, `RUEC8800`, `I#RUEC58` |
| Demais PGMs RUCA da cadeia alteracao, se impactados na montagem/serializacao | `CRUR3CY1`, copy COR0001 CRUR |
| | `RCOR*`, `TMNF*` — validacao em homologacao, sem alteracao de codigo |

- `RUEC8800` e chamado pelo RUCA8810 apenas como **fonte de dados**; `RUEC58-SIST-PROD` X(13) repassado por **MOVE direto** para `RUCA66-GP-COR0001-SIST-PRODC`.
- Validacao RCOR x TMNF x SICOR: cenario de **teste integrado**, nao escopo de implementacao desta entrega.

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| **RUCA000752** | Subida AGROLEG-1482 (SICOR 5.12) | `RUCA8810`, `I#RUCA66` | **Compilado** | `pgm/ruca8810.cbl`, `cpy/i#ruca66.cpy` |
| **RUCA000753** | Contingencia (versao PRD pre-1482) | `RUCA8810`, `I#RUCA66` | Pacote criado | `pgm/analise/ruca8810-baseline-1109.cbl`, `cpy/analise/i#ruca66-baseline-1109.cpy` |

- **Subida:** pacote **RUCA000752** promovido no Changeman; compile RC=0.
- **Contingencia:** pacote **RUCA000753** com fontes **baseline PRD** (1109) para retorno pos-implantacao — layout COR0001 pre-5.12 (`CodSistProdc` X(13), blocos 18–20 ativos, sem `NumOrdemDestcRenegc`).

## Promocao ambientes (Jira)

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| **TICKET-85280** | https://jira.bradesco.com.br:8443/browse/TICKET-85280 | RUCA000752 | RUCA000753 | Criado |

- Ticket Jira para promocao dos pacotes Changeman entre ambientes (TST / HOM / PRD conforme fluxo Bradesco).

## Requisito de negocio
- Iniciativa: SICOR 5.12 — ver `.cursor/skills/agroleg-knowledge/initiatives/SICOR-5.12.md`
- Epic Jira: AGROLEG-1098
- Mensagens COR/CIR desta historia: COR0001
- Regra de negocio resumida:
  1. **NumOrdemDestcRenegc** — incluir no grupo Renegociação; enviar sem preenchimento quando origem de recurso ≠ 100; quando = 100 (renegociação), enviar número da ordem de destinação do contrato renegociado (OBS Jira: por ora enviar `*`). Referência de padrão: Anexo C do levantamento 1109 (`NumOrdemDestcOr` na COR0005 / RUCA8814).
  2. **Exclusões da mensageria** — remover `Grupo_COR0001_CertifcOrgnco`, `Grupo_COR0001_RastlddEmpnmnt` e `PercEnerRenovvlEmpnmnt` (permanecem em tela e base).
  3. **Grupo Sistema Produção** — substituir `CodSistProdc` (13 dígitos) por `Grupo_COR0001_SistProdc` com 8 subcampos (`TpAgrctur`, `TpIntgcConsc`, `TpGraoSemte`, `TpIrrgc`, `TpCultvExplc`, `TpFaseCicloProdc`, `TpClima`, `TpOtrPratcManj`).
  4. Em homologacao, validar que a COR0001 gerada pelo RUCA transita corretamente ate o SICOR (sem alterar RCOR/TMNF nesta historia).

## Centros de custo impactados
- **RUCA** (unico CC de implementacao)

## Escopo tecnico
- COBOL: RUCA8810 (montador), I#RUCA66 (layout), serialização WRK-MSGEM-RURAL
- CICS: RUCA opção 33 — Ajustes SICOR — alteração de contrato
- DB2: tabelas de formalização/manutenção citadas no RUCA8810 (RUCAB015+, RUFIB002, etc.)
- BATCH / JCL / IMS: N/A (escopo online)

## Malha batch
- N/A — COR0001 gerada online após alteração de contrato no RUCA.

## Levantamento tecnico de referencia
- Documento: `Projetos/Capgemini/Finalizada/AGROLEG-1109/AGROLEG-1109_levantamento_tecnico_consolidado_RUEC_RUCA_CRUR.md` (v2.0)
- Contrato XSD: `COR0001.XSD` (copia baseline na raiz da demanda)
- Secao aplicavel: **§1 RUCA** (COR0001 alteracao); Anexo C (`NumOrdemDestcOr` COR0005 como referencia)

## Fontes impactados (confirmado)

Escopo de **alteracao de codigo** — somente RUCA (levantamento AGROLEG-1109 §1.2 / §1.6; PRD analisado em `pgm/` e `cpy/`):

| Objeto | Impacto | Alteracao |
|--------|---------|-----------|
| **RUCA8810** | **ALTO** | Montador COR0001 alteracao; eliminar `RUCA66-COD-SIST-PRODC`; popular 8 campos SistProdc a partir de `RUEC58-*` (CALL RUEC8800); ajustar ordem WRK-MSGEM-RURAL; parar serializacao certificacao/rastreabilidade/energia; incluir `NumOrdemDestcRenegc` |
| **I#RUCA66** | **ALTO** | Substituir `CodSistProdc` X(013) por grupo 8 campos; comentar blocos 18-20; incluir campo renegociacao |

Referencia / regressao (sem alteracao prevista nesta historia):

| Objeto | Papel |
|--------|-------|
| RUCA8812 | Envio ao RCOR — apenas teste pos-montagem |
| RUCA8806, RUCA8807 | Retorno/efetivacao |
| RUCA8814, I#RUCA67 | Referencia Anexo C (`NumOrdemDestcOr` COR0005) |

Fora desta historia: RUEC8810, I#RUEC66, RUEC8800, I#RUEC58, CRUR3CY1, RCOR*, TMNF*.

## Estrutura de pastas (componentes)

- **Escopo (alteracao):** `pgm/ruca8810.cbl`, `cpy/i#ruca66.cpy` (PRD baixado)
- **Baseline analise (1109):** `pgm/analise/*-baseline-1109.cbl`, `cpy/analise/*-baseline-1109.cpy` — **nao alterar**
- **Referencia:** `COR0001.XSD`, anexo Jira DRN COR0001

Ver `Projetos/_indice/guia-organizacao-seguranca.md`.

## Fontes pendentes baixa

Nenhuma pendencia de compilacao (COPY/DCLGEN resolvidas em LIBS). Opcional:

| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P3 | DOC | 5.12 - DRN-COR0001-*.docx | Layout regulatório (anexo Jira) | raiz demanda | opcional — `COR0001.XSD` v5.12 na raiz |

Referencia / baseline (sem alteracao): `pgm/analise/*-baseline-1109.cbl`, `cpy/analise/*-baseline-1109.cpy`.

## Impacto tecnico (confirmado)

- **Programas alterados:** RUCA8810 — MOVE SistProdc pos-5245; 5110 NumOrdemDestcRenegc; 5106/5425 exclusao certif/rastreab/energia
- **Copybooks alterados:** I#RUCA66 — grupo SistProdc 8 campos; NumOrdemDestcRenegc; blocos 18–20 comentados
- **Sections RUCA8810 impactadas (5.12):**
  - `5245-CHAMAR-RUEC8800` (~3030) — `CAPG05* -MOVE` para `RUCA66-COD-SIST-PRODC`; `MOVE RUEC58-SIST-PROD` → `RUCA66-GP-COR0001-SIST-PRODC` (X(13) direto)
  - `5110-FORMATAR-GRUPO-RENEGC` (~3388) — incluir `NumOrdemDestcRenegc` (provisorio `*` conforme Jira)
  - `5106-MONTA-CERTIFICACAO` (~3304) — deixar de serializar certificacao/rastreabilidade/energia (blocos 18–20)
  - Montagem principal `WRK-MSGEM-RURAL` (~2819–3377 e ~6306–6494) — remover moves COR0001-18/19/20; ajustar COR0001-9 (grupo SistProdc); conferir ordem vs `COR0001.XSD` v5.12
- **I#RUCA66 — gaps confirmados no PRD:**
  - `RUCA66-COD-SIST-PRODC` X(013) em COR0001-9 (~168) — substituir por grupo 8 campos
  - Grupo renegociacao COR0001-4 (~87) — falta `NumOrdemDestcRenegc`
  - Blocos COR0001-18/19/20 (~329–351) — certificacao, rastreabilidade, `PercEnerRenovvlEmpnmnt` ainda no copy
- **Mapeamento SistProdc:** `RUEC58-SIST-PROD` X(13) → `RUCA66-GP-COR0001-SIST-PRODC` (MOVE direto pos-5245); subcampos XSD mapeados byte a byte pelo RUEC8800

- **Tabelas DB2 principais (leitura — sem alteracao):** RUCAB015 (contrato/sist. prod.), RUFIB002, RURCB000/003/006/018/022/028, RUECB014/025/040, RUCAB004/012–019/023, TRAGB013
- **CALL externos (sem alteracao nesta historia):** RUEC8800, RUEC8824, MESU9032, CMRU307C, BRAD7600, RUCA8516, CRUR240G
- **Fluxo:** RUCA op.33 alteracao → RUCA8810 monta COR0001 → RUCA8812 → RCOR5005 → SICOR

## Plano de ajuste (proposta)

**Status: implementado** (CAPG05* — 05/2026). Detalhe abaixo permanece como referencia do que foi entregue.

### Resumo das mudancas 5.12

| Requisito Jira | Acao |
|----------------|------|
| Grupo SistProdc (8 campos) | Substituir `RUCA66-COD-SIST-PRODC` X(013) por 8 PIC X (total 13 bytes — LENGTH COR0001-9 inalterado) |
| NumOrdemDestcRenegc | Incluir X(003) no grupo renegociacao, apos `NUMREF-BCCOR-RENEGC`; provisorio `*` (Jira) |
| Exclusoes mensageria | Comentar blocos 18–20 no copy; remover serializacao (5106 + 5425) |
| DB2/tela | Manter leitura RUCAB015 em 5106 comentada — dados permanecem em base/tela |

### Tabela de implementacao

| # | Fonte | Area / linha ref. | Alteracao proposta | Referencia |
|---|-------|-------------------|-------------------|------------|
| 1 | I#RUCA66 | Cabecalho copy | Bloco CAPG05* — SICOR 5.12 AGROLEG-1482 | bradesco-style |
| 2 | I#RUCA66 | COR0001-4 (~87) | Incluir `RUCA66-NUM-ORDEM-DEST-RENEGC PIC X(003)` apos `NUMREF-BCCOR-RENEGC`, antes de `VLR-RENEGC` (ordem XSD §Renegc) | COR0001.XSD L762; quality #10 |
| 3 | I#RUCA66 | COR0001-9 (~168) | Comentar `RUCA66-COD-SIST-PRODC X(013)`; incluir 8 campos: `TP-AGRCTUR` X(001), `TP-INTGC-CONSC` X(001), `TP-GRAO-SEMTE` X(001), `TP-IRRGC` X(002), `TP-CULTV-EXPLC` X(002), `TP-FASE-CICLO-PRODC` X(002), `TP-CLIMA` X(002), `TP-OTR-PRATC-MANJ` X(002) | COR0001.XSD L830–911; levantamento 1109 §1.5 |
| 4 | I#RUCA66 | COR0001-18 (~329) | Comentar hierarquia completa `GRUPO-COR0001-CERTIFCOR` (CodCertif + URL) | SICOR 5.12 exclusoes; quality #10 |
| 5 | I#RUCA66 | COR0001-19 (~340) | Comentar hierarquia completa `GRUPO-COR0001-RASTLDDEM` | idem |
| 6 | I#RUCA66 | COR0001-20 (~350) | Comentar `RUCA66-PERCENERRENOVVLEMPNMNT` | idem |
| 7 | RUCA8810 | Pos-5245 (~3030) | `CAPG05* -MOVE RUEC58-SIST-PROD TO RUCA66-COD-SIST-PRODC`; `MOVE RUEC58-SIST-PROD TO RUCA66-GP-COR0001-SIST-PRODC` (X(13) direto) | cap. 03 + 05; levantamento 1109 §1.6 |
| 8 | RUCA8810 | 5110-FORMATAR-GRUPO-RENEGC (~3388) | Apos `NUMREF-BCCOR-RENEGC`, incluir `MOVE ALL '*' TO RUCA66-NUM-ORDEM-DEST-RENEGC` (provisorio Jira) | COR0001.XSD L762; Anexo C 1109 |
| 9 | RUCA8810 | 5105-FORMATAR-GRUPO-DESTC (~3295) | Comentar `-PERFORM 5106-MONTA-CERTIFICACAO`; apos COR0001-17 ir direto para `5107-MONTA-ZARC` (XSD L680 — sem certif/rastreab/energia entre STN e ZARC) | COR0001.XSD L670–688 |
| 10 | RUCA8810 | 5106-MONTA-CERTIFICACAO (~3304) | Comentar section inteira (DB2 + moves WRK-MSGEM 18/19/20) — manter codigo para auditoria | AGROLEG-1239 padrao |
| 11 | RUCA8810 | 5425-FORMATAR-MSG-CONT (~6437) | Comentar moves `COR0001-18`, `COR0001-19`, `COR0001-20` (continuacao de mensagem) | idem |
| 12 | RUCA8810 | Serializacao COR0001-9 (~3127, ~6377) | Sem alteracao de ordem — LENGTH COR0001-9 preservado (13 bytes SistProdc); validar payload pos-implementacao vs XSD | levantamento 1109 §1.8 |

### Mapeamento SistProdc (RUEC8800 → RUCA66)

`RUEC58-SIST-PROD` (`PIC X(13)`) montado pelo RUEC8800; no RUCA8810, **MOVE direto** para `RUCA66-GP-COR0001-SIST-PRODC` (13 bytes, layout byte a byte preservado). Sem decomposicao campo a campo no PGM.

### Sequencia XSD 5.12 — pontos de atencao na serializacao

Ordem atual RUCA8810 apos COR0001-17: **18 → 19 → 20 → 21 (ZARC) → 21A → …**

Ordem XSD 5.12 apos `CodSTNEqulzc`: **Grupo_ZARC → PercBonusCAR → CNPJBaseInstExigldd → PercBonusSustvl → TpConfdd → Librc → Cooperd**

Acao do plano: remover 18/19/20 da cadeia; sequencia 17 → 5107 (21) → 21A… permanece aderente. Validar em teste comparativo payload vs `COR0001.XSD` v5.12.

### Ordem de implementacao

1. **I#RUCA66** — itens 1–6 (layout; compilar copy isolado se possivel)
2. **RUCA8810** — itens 7–12 (montagem + serializacao; compilar PGM)
3. **Validacao estatica** — comparar LENGTH grupos alterados; checklist SICOR 5.12

### Riscos

| Risco | Mitigacao |
|-------|-----------|
| Conversao RUEC58 → grupo SistProdc | MOVE direto X(13); validar 8 subcampos vs payload legado em teste |
| LENGTH COR0001-4 aumenta (+3) por NumOrdemDestcRenegc | Esperado; revisar ACU-POSICAO em 5110 e 5425 |
| NumOrdemDestcRenegc provisorio `*` | Aceite Jira; evolucao futura quando origem recurso = 100 |
| 5425 (msg continuacao) divergente do fluxo principal | Alterar ambos os caminhos (itens 9–11) |
| RCOR/TMNF fora do escopo | Teste integrado homologacao pos-subida RUCA |

### Cenarios de teste (preliminar)

- [ ] Alteracao contrato RUCA op.33 — COR0001 aceita SICOR; grupo SistProdc com 8 subcampos no payload
- [ ] Payload sem tags certificacao/rastreabilidade/energia (blocos 18–20 ausentes)
- [ ] Grupo renegociacao com `NumOrdemDestcRenegc` = `***` (provisorio)
- [ ] Contrato sem glebas / com WKTPart — regressao glebas (7C2510)
- [ ] Contrato sem coordenadas geodesicas — municipio vs gleba
- [ ] Regressao: certificacao/rastreabilidade/energia permanecem em RUCAB015 e tela
- [ ] Msg continuacao (5425) — mesmo layout 5.12 do fluxo principal
- [ ] Validacao estrutural `COR0001.XSD` v5.12 (payload antes/depois)
- [ ] Negativo: subcampo SistProdc invalido (ex. TpIrrgc ≠ 2 digitos)

## Decisoes tecnicas
- **Escopo restrito ao RUCA** — unicos fontes alterados: `RUCA8810` e `I#RUCA66` (confirmado negocio e levantamento 1109 §1.2).
- Sistema de producao: **MOVE direto** `RUEC58-SIST-PROD` → `RUCA66-GP-COR0001-SIST-PRODC` (pos-5245); sem alteracao em `RUEC8800` ou `I#RUEC58`.
- SistProdc no `I#RUCA66`: grupo logico `RUCA66-GP-COR0001-SIST-PRODC` (8 subcampos, 13 bytes, **sem tag `{xxxx}`** — nao repetivel); nomes `RUCA66-TP-*` inalterados para o PGM.
- `NumOrdemDestcRenegc`: envio provisorio `*` em 5110; referencia Anexo C 1109 para evolucao futura.
- Certificacao/rastreabilidade/energia: section 5106 comentada; dados permanecem em RUCAB015/tela.
- Rastreio alteracao: prefixo **CAPG05\*** (SICOR 5.12).
- RCOR/TMNF: apenas cenario de teste em homologacao.

## Historias relacionadas
| Historia | Relacao | Observacao |
|---|---|---|
| AGROLEG-1098 | pre-requisito | Epic SICOR 5.12 |
| AGROLEG-1109 | mesma-mensagem | Levantamento v2.0 — §1 RUCA, Anexo B/C |
| AGROLEG-1239 | complementa | COR0005 5.12 — exclusao certificacao/rastreabilidade |
| AGROLEG-1243 | complementa | COR0001E — escopo RCOR, nao desta historia |
| AGROLEG-1520, 1524, 1537, 1541, 1574 | complementa | Subtarefas RUCA |

## Licoes para proximas analises
- Levantamento 1109 elimina suposicao RUEC8821/I#RUECC1 — COR0001 **alteracao** e RUCA8810 + I#RUCA66.
- Reutilizar padrao AGROLEG-1239 (comentar hierarquia, campos excluidos permanecem em tela).
- Anexo C 1109: `NumOrdemDestcOr` fixo `'001'` na COR0005 — referencia para `NumOrdemDestcRenegc`.

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - CMRUW09I -> LIBS/COPYLIB/CMRU/cmruw09i.cpy
  - CRURW00W -> LIBS/COPYLIB/CRUR/crurw00w.cpy
  - CRURWG0I -> LIBS/COPYLIB/CMRU/crurwg0i.cpy
  - CRURWG3Y -> LIBS/COPYLIB/CRUR/crurwg3y.cpy
  - CRURWPGI -> LIBS/COPYLIB/CRUR/crurwpgi.cpy
  - I#BRAD7C -> LIBS/COPYLIB/BRAD/I#BRAD7C.cpy
  - I#RCOR10 -> LIBS/COPYLIB/RCOR/i#rcor10.cpy
  - I#RUCA66 -> Projetos/Capgemini/AGROLEG-1482/cpy/i#ruca66.cpy
  - I#RUCAA3 -> LIBS/COPYLIB/RUCA/i#rucaa3.cpy
  - I#RUEC58 -> LIBS/COPYLIB/CMRU/i#ruec58.cpy
  - RUECW824 -> LIBS/COPYLIB/RUEC/ruecw824.cpy
- DCLGEN (encontradas):
  - RUCAB004 -> LIBS/DCLGEN/RUCA/rucab004.cpy
  - RUCAB012 -> LIBS/DCLGEN/RUCA/rucab012.cpy
  - RUCAB013 -> LIBS/DCLGEN/RUCA/rucab013.cpy
  - RUCAB014 -> LIBS/DCLGEN/RUCA/rucab014.cpy
  - RUCAB015 -> LIBS/DCLGEN/RUCA/rucab015.cpy
  - RUCAB016 -> LIBS/DCLGEN/RUCA/rucab016.cpy
  - RUCAB017 -> LIBS/DCLGEN/RUCA/rucab017.cpy
  - RUCAB018 -> LIBS/DCLGEN/RUCA/rucab018.cpy
  - RUCAB019 -> LIBS/DCLGEN/RUCA/rucab019.cpy
  - RUCAB023 -> LIBS/DCLGEN/RUCA/rucab023.cpy
  - RUECB014 -> LIBS/DCLGEN/RUEC/ruecb014.cpy
  - RUECB025 -> LIBS/DCLGEN/RUEC/ruecb025.cpy
  - RUECB040 -> LIBS/DCLGEN/RUEC/ruecb040.cpy
  - RUECB09G -> LIBS/DCLGEN/RUEC/ruecb09g.cpy
  - RUECB09J -> LIBS/DCLGEN/RUEC/ruecb09j.cpy
  - RUECB09N -> LIBS/DCLGEN/RUEC/ruecb09n.cpy
  - RUFIB002 -> LIBS/DCLGEN/RUFI/rufib002.cpy
  - RURCB000 -> LIBS/DCLGEN/RURC/rurcb000.cpy
  - RURCB003 -> LIBS/DCLGEN/RURC/rurcb003.cpy
  - RURCB004 -> LIBS/DCLGEN/RURC/rurcb004.cpy
  - RURCB006 -> LIBS/DCLGEN/RURC/rurcb006.cpy
  - RURCB018 -> LIBS/DCLGEN/RURC/rurcb018.cpy
  - RURCB022 -> LIBS/DCLGEN/RURC/rurcb022.cpy
  - RURCB028 -> LIBS/DCLGEN/RURC/rurcb028.cpy
  - RURCB082 -> LIBS/DCLGEN/RURC/rurcb082.cpy
  - RURCB083 -> LIBS/DCLGEN/RURC/rurcb083.cpy
  - RURCB084 -> LIBS/DCLGEN/RURC/rurcb084.cpy
  - RURCB085 -> LIBS/DCLGEN/RURC/rurcb085.cpy
  - RURCB086 -> LIBS/DCLGEN/RURC/rurcb086.cpy
  - RURCB087 -> LIBS/DCLGEN/RURC/rurcb087.cpy
  - RURCB088 -> LIBS/DCLGEN/RURC/rurcb088.cpy
  - RURCB089 -> LIBS/DCLGEN/RURC/rurcb089.cpy
  - RURCB090 -> LIBS/DCLGEN/RURC/rurcb090.cpy
  - RURCB091 -> LIBS/DCLGEN/RURC/rurcb091.cpy
  - RURCV002 -> LIBS/DCLGEN/RURC/rurcv002.cpy
  - SQLCA -> LIBS/DCLGEN/sqlca.cpy
  - TRAGB013 -> LIBS/DCLGEN/TRAG/tragb013.cpy
- Pendencias (nao encontradas em LIBS):
  - Nenhuma
- Dependencias de sistema (CALL — sem alteracao nesta historia):
  - RUEC8800 (sistema produtivo — I#RUEC58)
  - RUEC8824, MESU9032, CMRU307C, BRAD7600, RUCA8516, CRUR240G
  - RCOR5005, TMNF, SICOR BACEN (integracao — homologacao)

## Riscos e atencoes
- Rejeicao COR0001 por ordem incorreta de tags (mitigacao: comparativo sequencia vs XSD).
- Envio de tags excluidas 5.12 ainda presentes em I#RUCA66 blocos 18-20.
- NumOrdemDestcRenegc: Jira indica `*` provisorio.
- RCOR/TMNF: teste integrado em homologacao; codigo fora do escopo RUCA-only.
- Anexo D 1109: RUCA8810 tem campos CNPJ numericos — verificar se demanda futura CNPJ alfanumerico impacta.

## Plano de testes
- [ ] Alterar contrato RUCA op. 33 sem coordenadas geodésicas — COR0001 layout 5.12
- [ ] Alterar contrato com coordenadas financiadas e não cultivadas — COR0001 layout 5.12
- [ ] Alterar contrato e protocolar no SICOR BACEN — nova COR0001
- [ ] Validacao SICOR 5.12 (checklist em initiatives/SICOR-5.12.md)
- [ ] Regressao: campos excluidos da mensagem permanecem em tela e base
- [ ] Payload antes/depois + validacao estrutural COR0001.XSD v5.12

## Evidencias
- Pacote Changeman **RUCA000752** — subida `RUCA8810` + `I#RUCA66`; compilado OK
- Pacote contingencia **RUCA000753** — versao PRD baseline-1109
- Ticket promocao **[TICKET-85280](https://jira.bradesco.com.br:8443/browse/TICKET-85280)**
- (a preencher pos-homologacao: payload COR0001, aceite SICOR, prints)

## Dados de origem
- Consultoria: Capgemini
- Canal: Jira XML
- Referencia externa: AGROLEG-1482
- Chave Jira (Capgemini): AGROLEG-1482
- Titulo: [Flash - RUCA] - SICOR 5.12 - Alteração do Contrato (COR0001)
- Status: Em Análise
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Mon, 25 May 2026 12:17:22 -0300

## Dados Jira (sync)
- Chave: AGROLEG-1482
- Titulo: [Flash - RUCA] - SICOR 5.12 - Alteração do Contrato (COR0001)
- Status: Em Análise
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Mon, 25 May 2026 12:17:22 -0300

## Status
- [x] Em andamento
- [ ] Em teste
- [x] Pronto para subida
- [x] Em homologacao
- [ ] Implantado
