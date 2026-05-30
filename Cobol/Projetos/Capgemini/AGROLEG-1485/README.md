# AGROLEG-1485 - SICOR 5.12 - Alteração do Contrato (COR0005)

## Status workflow
- Fase atual: Desenvolvimento concluido — aguardando pre-subida
- Ultimo comando: Implementar plano AGROLEG-1485
- Proximo passo: `Pre-subida AGROLEG-1485`

## Objetivo
- Eu como PO
- Quero adequar o processo de geração da mensageria **COR0005**, na **alteração de contratos** (RUCA opção 33 — Ajustes SICOR / subopção 01), para excluir campos descontinuados na versão 5.12 do Catálogo SICOR BACEN.
- Para garantir conformidade regulatória no envio ao BACEN, com comunicação padronizada e integridade dos dados transmitidos.
- Campos a **excluir da mensageria** (permanecem em telas/base RUCA–RUEC–CRUR):
  - Certificação Orgânica (`Grupo_COR0005_CertificOrgnco`)
  - Rastreabilidade Empreendimento (`Grupo_COR0005_RastlddEmpnmnt`)
  - Percentual Energia Renovável Empreendimento (`PercEnerRenovvlEmpnmnt`)
  - Indicador Confirmação Alerta Sustentabilidade (`IndrConfcAlertSustldd`)

## Restricao de escopo (confirmado — RUCA online)

Esta historia segue o mesmo padrão da **AGROLEG-1482** (COR0001 alteração): escopo **somente RUCA** no fluxo online de alteração de contrato.

| Incluido (confirmado) | Excluido (outras historias) |
|---------------------|----------------------------|
| `RUCA8814`, `I#RUCA67` | `RUEC8831`, `I#RUECC5`, `RUEC0895` — **AGROLEG-1239** (COR0005 batch liberação) |
| Cadeia envio/retorno RUCA (teste) | `RCOR*`, `TMNF*` — validação integrada em homologação |
| | `CRUR*` — sem alteração prevista nesta entrega |

- Fonte confirma: **sem** `RUEC8831` nem outros montadores batch — apenas `RUCA8814` monta COR0005 alteração.
- `RUCA8812` (envio RCOR) permanece só para regressão; sem alteração nesta história.

## Requisito de negocio
- Iniciativa: SICOR 5.12 — ver `.cursor/skills/agroleg-knowledge/initiatives/SICOR-5.12.md`
- Epic Jira: AGROLEG-1098
- Mensagens COR/CIR desta historia: **COR0005** (retorno **COR0005R1** condiciona atualização de base)
- Regra de negocio resumida:
  1. Após alteração de contrato, enviar COR0005 com layout 5.12 (sem os quatro campos/grupos excluídos).
  2. Campos suprimidos da mensageria **mantidos** em telas e formulários RUCA opção 33 (e demais módulos conforme regras existentes).
  3. Critério de aceite: layout gerado conforme catálogo 5.12; validação protocolando mensagem junto ao BACEN.

## Centros de custo impactados
- **RUCA** (implementação provável)
- RUEC / RCOR — somente leitura ou teste integrado (sem alteração prevista nesta entrega)

## Escopo tecnico
- COBOL: montador COR0005 alteração (`RUCA8814` — 3438 linhas), layout (`I#RUCA67` — 315 linhas)
- CICS: RUCA opção 33 — Menu Ajustes SICOR — subopção 01 (alteração)
- DB2: tabelas de contrato/produtividade citadas no fluxo (confirmar no fonte — ex. `RUFIB002` no Anexo C 1109)
- BATCH / JCL / IMS: N/A nesta entrega (batch COR0005 → AGROLEG-1239)

## Malha batch
- N/A — COR0005 desta história gerada no fluxo **online** de alteração de contrato.
- Referência batch COR0005 liberação: `Projetos/_indice/referencia-batch-malha/malhas/AGROLEG-1239-liberacao.json` (regressão cruzada, se necessário).

## Levantamento tecnico de referencia
- Documento: `Projetos/Capgemini/Finalizada/AGROLEG-1109/AGROLEG-1109_levantamento_tecnico_consolidado_RUEC_RUCA_CRUR.md` (v2.0)
- Contrato XSD: `COR0005.XSD` (cópia v5.12 na raiz da demanda, origem AGROLEG-1109)
- Seção aplicável: **Anexo C** (`NumOrdemDestcOr`, `Grupo_COR0005_DestcFincmnt`); paralelo implementado em AGROLEG-1482 §1 (COR0001)

## Fontes impactados (confirmado)

| Objeto | Impacto | Alteracao provável |
|--------|---------|-------------------|
| **RUCA8814** | **ALTO** | Montador COR0005 alteração; deixar de serializar certificação, rastreabilidade, energia renovável e indicador confirmação alerta sustentabilidade; ajustar ordem `WRK-MSGEM-RURAL` vs XSD 5.12 |
| **I#RUCA67** | **ALTO** | Comentar hierarquia dos grupos/campos excluídos 5.12 no layout físico COR0005 |

Referência / regressão (sem alteração prevista nesta história):

| Objeto | Papel |
|--------|-------|
| RUCA8812 | Envio ao RCOR — teste pós-montagem |
| RUCA8810, I#RUCA66 | Paralelo COR0001 — AGROLEG-1482 (mesmo fluxo op. 33) |
| RUEC8831, I#RUECC5 | COR0005 batch — AGROLEG-1239 |

## Estrutura de pastas (componentes)

- **Escopo (alteração):** `pgm/ruca8814.cbl`, `cpy/i#ruca67.cpy` — baixados (idênticos ao baseline PRD no momento da baixa)
- **Baseline analise:** `pgm/analise/`, `cpy/analise/` — versão PRD pré-demanda — **não alterar**
- **Referência:** `COR0005.XSD`, `Alteracoes512v511.xls`, anexo Jira `5.12 - DRN-COR0001-COR0003R1-COR0005-COR0006R1.docx`

Ver `Projetos/_indice/guia-organizacao-seguranca.md`.

## Fontes baixados

| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | PGM | RUCA8814 | Montador COR0005 alteração (Anexo C / 1109) | pgm/ | baixado |
| P1 | CPY | I#RUCA67 | Layout COR0005 | cpy/ | baixado |
| P2 | PGM | RUCA8814 | Baseline PRD pré-5.12 | pgm/analise/ | baixado |
| P2 | CPY | I#RUCA67 | Baseline PRD pré-5.12 | cpy/analise/ | baixado |
| P3 | PGM | RUCA8812 | Cadeia envio — regressão | pgm/analise/ | baixado |
| P3 | DOC | 5.12 - DRN-COR0001-COR0003R1-COR0005-COR0006R1.docx | Layout regulatório (anexo Jira) | raiz demanda | opcional — `COR0005.XSD` já na raiz |

## Impacto tecnico (confirmado)

Escopo validado no fonte: **2 objetos** (`RUCA8814`, `I#RUCA67`). Planilha `Alteracoes512v511.xls`: COR0005 — **0 inclusões**, **10 exclusões** (campos abaixo), **57 movimentações** de sequência XSD. **Sem SistProdc** (diferente do COR0001 / AGROLEG-1482).

### I#RUCA67 — comentar hierarquia excluída 5.12

| Grupo / campo | Linha copy | XSD / tag |
|---|---|---|
| `RUCA67-IND-CONF-ALRT-SUST` em `COR0005-1` | ~76–77 | `IndrConfcAlertSustldd` |
| `RUCA67-COR0005-11` (certificação orgânica) | ~227–234 | `Grupo_COR0005_CertificOrgnco` |
| `RUCA67-COR0005-12` (rastreabilidade) | ~236–243 | `Grupo_COR0005_RastlddEmpnmnt` |
| `RUCA67-COR0005-13` (energia renovável) | ~245–249 | `PercEnerRenovvlEmpnmnt` |

**Manter:** `RUCA67-COR0005-14` (`CodSTNEqulzc` / STN) — ~251–253.

### RUCA8814 — deixar de popular/serializar campos excluídos

| Ponto | Linha PGM | Acao |
|---|---|---|
| `MOVE 'N' TO RUCA67-IND-CONF-ALRT-SUST` | ~1117 | Comentar (`CAPG05*`) — campo sai de `COR0005-1` |
| `MOVE RUCA67-COR0005-1` (bloco cabeçalho) | ~1119–1124 | Ajuste automático via copy (1 byte a menos) |
| `PERFORM 5260-FORMATAR-GRUPO-CORGN` | ~1205 | Manter chamada — section trata também STN |
| Section `5260-FORMATAR-GRUPO-CORGN` | ~3278–3343 | Comentar blocos certif/rastreab/energia (~3281–3326); **manter** STN / `COR0005-14` (~3328–3340) |

**Cuidado:** não comentar a section 5260 inteira — ela serializa `COR0005-14` (STN) após os grupos excluídos.

### Dependencias e regressão

- DCLGEN `RUFIB093` referenciado no PGM (~563, section 5050) — **pendente em LIBS** (pré-existente; não bloqueia escopo 5.12).
- `RUCA8812` em `pgm/analise/` — cadeia envio RCOR; sem alteração prevista.
- Demais groups (`COR0005-15` desclassificação, `COR0005-16/17`) já comentados/inativos no baseline — fora desta entrega.

### Implementacao (05/2026)

| Fonte | Alteracao aplicada |
|-------|-------------------|
| `cpy/i#ruca67.cpy` | Cabecalho CAPG05; comentado `IND-CONF-ALRT-SUST`; comentados `COR0005-11/12/13`; mantido `COR0005-14` (STN) |
| `pgm/ruca8814.cbl` | Cabecalho CAPG05; comentado MOVE indicador (~1125); comentados blocos certif/rastreab/energia em 5260 (~3289–3334); STN ativo (~3336–3348) |

## Plano de ajuste (proposta)

**Status: implementado** (CAPG05* — 05/2026). Detalhe abaixo permanece como referencia do que foi entregue.

### Resumo das mudancas 5.12

| Requisito Jira | Acao |
|----------------|------|
| `IndrConfcAlertSustldd` | Comentar campo em `COR0005-1` (copy); comentar `MOVE 'N'` no PGM (~1117) |
| `Grupo_COR0005_CertificOrgnco` | Comentar `COR0005-11` no copy; comentar população + serialização em 5260 (~3281–3295) |
| `Grupo_COR0005_RastlddEmpnmnt` | Comentar `COR0005-12` no copy; comentar população + serialização em 5260 (~3297–3311) |
| `PercEnerRenovvlEmpnmnt` | Comentar `COR0005-13` no copy; comentar população + serialização em 5260 (~3313–3326) |
| `CodSTNEqulzc` (STN) | **Manter** — `COR0005-14` no copy; bloco STN em 5260 (~3328–3340) inalterado |
| DB2 / tela | **Manter** SELECT `5010-SELECT-RUCAB015` e tratamento NULL (~929–1024) — dados permanecem em `TMANUT_CONTR_RECOR` e telas |

### Tabela de implementacao

| # | Fonte | Area / linha ref. | Alteracao proposta | Referencia |
|---|-------|-------------------|-------------------|------------|
| 1 | I#RUCA67 | Cabecalho copy | Bloco `CAPG05*` — SICOR 5.12 AGROLEG-1485; atualizar versao catalogo 5.12 | bradesco-style |
| 2 | I#RUCA67 | COR0005-1 (~76–77) | Comentar hierarquia `RUCA67-IND-CONF-ALRT-SUST PIC X(001)` (`-05`/`-PIC`); `LENGTH OF COR0005-1` reduz 1 byte | COR0005.XSD v5.12; quality #10 |
| 3 | I#RUCA67 | COR0005-11 (~227–234) | Comentar arvore completa certificacao (`GRUPO-COR0005-CORGN`, `CCETFC-ORGNC`, `EURL-CETFC-ORGNC`) | SICOR 5.12 exclusoes; quality #10 |
| 4 | I#RUCA67 | COR0005-12 (~236–243) | Comentar arvore completa rastreabilidade (`GRUPO-COR0005-RASTB`, `CRSTRB-EMPTO`, `EURL-RSTRB`) | idem |
| 5 | I#RUCA67 | COR0005-13 (~245–249) | Comentar `PENERG-RENOV` + `REDEFINES PENERG-RENOV-N` (linha 249 ativa hoje — incluir na arvore) | idem |
| 6 | RUCA8814 | Cabecalho PGM (~47+) | Bloco `CAPG05*` — objetivo exclusao COR0005 5.12 | bradesco-style |
| 7 | RUCA8814 | Montagem cabecalho (~1117) | `CAPG05* -MOVE 'N' TO RUCA67-IND-CONF-ALRT-SUST` | cap. 03; COR0005.XSD |
| 8 | RUCA8814 | 5260-FORMATAR-GRUPO-CORGN (~3281–3295) | Comentar IF/MOVE certificacao + `MOVE COR0005-11` / ADD ACU-POSICAO | AGROLEG-1482 padrao 5106 |
| 9 | RUCA8814 | 5260 (~3297–3311) | Comentar IF/MOVE rastreabilidade + `MOVE COR0005-12` / ADD | idem |
| 10 | RUCA8814 | 5260 (~3313–3326) | Comentar IF/MOVE energia + `MOVE COR0005-13` / ADD | idem |
| 11 | RUCA8814 | 5260 (~3328–3340) | **Sem alteracao** — STN / `COR0005-14` permanece apos exclusoes | COR0005.XSD L360 |
| 12 | RUCA8814 | Chamada ~1205 | **Manter** `PERFORM 5260-FORMATAR-GRUPO-CORGN` (section ainda serializa STN) | — |
| 13 | RUCA8814 | 5010-SELECT-RUCAB015 (~929–1024) | **Sem alteracao** — leitura DB2 certif/rastreab/energia mantida (dados em base) | regra negocio Jira |
| 14 | Ambos | Pos-implementacao | Validar `LENGTH OF` grupos alterados; payload vs `COR0005.XSD` v5.12 | checklist SICOR 5.12 |

### Sequencia XSD 5.12 — pontos de atencao na serializacao

Ordem **5.11** apos `Grupo_COR0005_Propt`: **CertificOrgnco → RastlddEmpnmnt → PercEnerRenovvl → CodSTNEqulzc → …**

Ordem **5.12** apos `Grupo_COR0005_Propt`: **`CodSTNEqulzc` → `Grupo_COR0005_DesclassTot` → …** (sem certif/rastreab/energia; sem `IndrConfcAlertSustldd` no cabecalho)

Acao do plano: apos `5250-FORMATAR-GRUPO-PROPT` (`COR0005-10`), ir direto para STN em 5260; demais grupos (`COR0005-15` desclass, `COR0005-16/17`) ja inativos no baseline PRD.

**Diferenca vs AGROLEG-1482:** COR0005 nao possui caminho de msg continuacao (5425); exclusoes concentradas em 5260 + cabecalho. **Sem SistProdc** nem `NumOrdemDestcRenegc`.

### Ordem de implementacao

1. **I#RUCA67** — itens 1–5 (layout; compilar copy isolado se possivel)
2. **RUCA8814** — itens 6–12 (cabecalho + section 5260 parcial; compilar PGM)
3. **Validacao estatica** — item 14; checklist quality code Hexavision (cap. 03, 08, 10)

### Riscos

| Risco | Mitigacao |
|-------|-----------|
| Comentar section 5260 inteira por engano | Plano explicita manter bloco STN (~3328–3340); revisar diff vs `pgm/analise/` |
| `LENGTH OF COR0005-1` reduz (-1 byte) | Esperado; `MOVE COR0005-1` usa LENGTH OF — auto-ajuste |
| Groups 11–13 com linhas `.` mistas no copy | Comentar arvore completa com `CAPG05*` incluindo REDEFINES linha 249 |
| SELECT 5010 ainda le campos excluidos | Intencional — exclusao so da mensageria; dados DB2/tela intactos |
| Ordem tags pos-Propt | Validar payload pos-STN vs XSD (DesclassTot/Parcl ja comentados no PRD) |
| Sobreposicao AGROLEG-1239 | Escopo restrito RUCA8814/I#RUCA67 |

### Cenarios de teste (preliminar)

- [ ] Alteracao contrato RUCA op. 33 — COR0005 aceita SICOR; payload aderente `COR0005.XSD` v5.12
- [ ] Payload **sem** tags certificacao, rastreabilidade, energia e `IndrConfcAlertSustldd`
- [ ] Payload **com** `CodSTNEqulzc` quando preenchido em `RUCAB015`
- [ ] Contrato com glebas / WKTPart — regressao `5250`/`5254` (7C2510)
- [ ] Retorno COR0005R1 — aceite e rejeicao BACEN; atualizacao base
- [ ] Regressao: certificacao/rastreabilidade/energia permanecem em `TMANUT_CONTR_RECOR` e tela RUCA
- [ ] Validacao estrutural payload vs `COR0005.XSD` v5.12 (comparar com baseline PRD pre-alteracao)
- [ ] Cadeia envio `RUCA8812` → RCOR — smoke test pos-montagem

## Decisoes tecnicas
- **Escopo restrito ao RUCA** — unicos fontes alterados: `RUCA8814` e `I#RUCA67` (confirmado fonte e analise Fase 2).
- **Prefixo rastreio:** `CAPG05*` em todas as linhas comentadas desta demanda; verbos com `-` (`-MOVE`, `-IF`, `-ADD`); **nao excluir** linhas (cap. 03).
- **Section 5260 parcial:** blocos certif/rastreab/energia comentados (~3289–3334); STN / `COR0005-14` (~3336–3348) mantido ativo.
- **SELECT 5010 mantido:** leitura DB2 dos campos excluidos permanece (dados em base/tela); exclusao so da mensageria.
- **Sem alteracao em** `RUEC8831`, `RUCA8812`, `pgm/analise/`, `cpy/analise/`.
- **Implementado 05/2026:** cabecalhos CAPG05 em `I#RUCA67` e `RUCA8814`; copy comentou `IND-CONF-ALRT-SUST` + `COR0005-11/12/13`; PGM comentou MOVE indicador e section 5260 (certif/rastreab/energia).

## Historias relacionadas
| Historia | Relacao | Observacao |
|---|---|---|
| AGROLEG-1098 | pre-requisito | Epic SICOR 5.12 |
| AGROLEG-1109 | mesma-mensagem | Levantamento consolidado; Anexo C COR0005 |
| AGROLEG-1482 | complementa | Mesmo fluxo RUCA op. 33 — COR0001 alteração (RUCA8810 / I#RUCA66) |
| AGROLEG-1239 | mesma-mensagem | COR0005 batch RUEC (RUEC8831 / I#RUECC5) — não duplicar escopo |
| AGROLEG-1547 | complementa | Subtarefa Jira |
| AGROLEG-1551 | complementa | Subtarefa Jira |
| AGROLEG-1552 | complementa | Subtarefa Jira |
| AGROLEG-1553 | complementa | Subtarefa Jira |
| AGROLEG-1554 | complementa | Subtarefa Jira |
| AGROLEG-1576 | complementa | Subtarefa Jira |

## Licoes para proximas analises
- Reutilizar padrão de implementação da AGROLEG-1482 (exclusão blocos 18–20 equivalentes no COR0005).
- Anexo C 1109: `RUCA67-NRO-ORDEM-DEST` fixo `'001'` — validar aderência 5.12 e maxOccurs 60 vs comentário `[0..50]` no copy.

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - I#BRAD7C -> LIBS/COPYLIB/BRAD/I#BRAD7C.cpy
  - I#RCOR10 -> LIBS/COPYLIB/RCOR/i#rcor10.cpy
  - I#RUCA67 -> Projetos/Capgemini/AGROLEG-1485/cpy/i#ruca67.cpy
  - I#RUCA67 -> Projetos/Capgemini/AGROLEG-1485/cpy/analise/i#ruca67.cpy
- DCLGEN (encontradas):
  - RUCAB012 -> LIBS/DCLGEN/RUCA/rucab012.cpy
  - RUCAB013 -> LIBS/DCLGEN/RUCA/rucab013.cpy
  - RUCAB015 -> LIBS/DCLGEN/RUCA/rucab015.cpy
  - RUCAB016 -> LIBS/DCLGEN/RUCA/rucab016.cpy
  - RUCAB017 -> LIBS/DCLGEN/RUCA/rucab017.cpy
  - RUCAB018 -> LIBS/DCLGEN/RUCA/rucab018.cpy
  - RUCAB019 -> LIBS/DCLGEN/RUCA/rucab019.cpy
  - RUECB040 -> LIBS/DCLGEN/RUEC/ruecb040.cpy
  - RUECB048 -> LIBS/DCLGEN/RUEC/ruecb048.cpy
  - RUECB09N -> LIBS/DCLGEN/RUEC/ruecb09n.cpy
  - RUFIB002 -> LIBS/DCLGEN/RUFI/rufib002.cpy
  - RUFIB044 -> LIBS/DCLGEN/RUFI/rufib044.cpy
  - RUFIB092 -> LIBS/DCLGEN/RURC/rufib092.cpy
  - RURCB000 -> LIBS/DCLGEN/RURC/rurcb000.cpy
  - RURCB00A -> LIBS/DCLGEN/RURC/rurcb00a.cpy
  - RURCB028 -> LIBS/DCLGEN/RURC/rurcb028.cpy
  - RURCB097 -> LIBS/DCLGEN/RURC/rurcb097.cpy
  - SQLCA -> LIBS/DCLGEN/sqlca.cpy
  - TRAGB013 -> LIBS/DCLGEN/TRAG/tragb013.cpy
- Pendencias (nao encontradas em LIBS):
  - RUFIB093
- Dependencias de sistema:
  - Nenhuma

## Riscos e atencoes
- Sobreposição com AGROLEG-1239 (batch) e subtarefas online 1239 — manter escopo RUCA8814/I#RUCA67 nesta história.
- Ordem de tags COR0005 5.12 ≠ 5.11 — validar serialização completa vs `COR0005.XSD`.
- Texto Jira mistura batch e online — confirmar escopo exato com PO se surgir PGM fora de RUCA8814.
- Retorno COR0005R1 condiciona persistência — cenário de teste deve cobrir aceite e rejeição BACEN.

## Plano de testes
- [ ] Alteração de contrato RUCA op. 33 — geração COR0005 layout 5.12
- [ ] Protocolo mensagem junto ao BACEN (critério de aceite Jira)
- [ ] Retorno COR0005R1 — atualização de base conforme retorno
- [ ] Regressão: liberação de recursos / alteração taxa juros (disparos alternativos citados no Jira)
- [ ] Validacao SICOR 5.12 — checklist em `initiatives/SICOR-5.12.md`

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| | Subida (implementacao) | | pendente | `pgm/`, `cpy/` |
| | Contingencia (PRD pre-demanda) | | pendente | `pgm/analise/`, `cpy/analise/` |

## Promocao ambientes (Jira)

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| | | | | pendente |

## Evidencias
- (a preencher)

## Dados de origem
- Consultoria: Capgemini
- Canal: Jira XML (`AGROLEG-1485.xml`)
- Referencia externa: Sprint FLASH-RT2 2026 - -S4; Story Points 8.0; Métrica BIA 3.31
- Chave Jira (Capgemini): AGROLEG-1485
- Titulo: [Flash - RUCA] - SICOR 5.12 - Alteração do Contrato (COR0005)
- Status: Em Análise
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Mon, 25 May 2026 12:18:17 -0300

## Dados Jira (sync)
- Chave: AGROLEG-1485
- Titulo: [Flash - RUCA] - SICOR 5.12 - Alteração do Contrato (COR0005)
- Status: Em Análise
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Mon, 25 May 2026 12:18:17 -0300

## Status
- [x] Em andamento
- [ ] Em teste
- [ ] Pronto para subida
- [ ] Em homologacao
- [ ] Implantado
