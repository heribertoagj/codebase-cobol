# AGROLEG-1239 - SICOR 5.12 - Liberacao de contratos (COR0005 batch)

## Status workflow
- Fase atual: **Implantacao PRD** — pacotes RUEC001652 / RUEC001658
- Ultimo comando: Tickets implantacao TICKET-87532 (RUEC001652) + TICKET-87535 (RUEC001658)
- Proximo passo: Promocao PRD + validacao pos-implantacao COR0005 batch

## Objetivo
- Adequar o processo de geração da mensageria **COR0005** para excluir os campos
  descontinuados na versão 5.12 do Catálogo SICOR BACEN, garantindo conformidade
  regulatória no envio ao BACEN.
- Campos a excluir do layout COR0005:
  - Certificação Orgânica (`Grupo_COR0005_CertificOrgnco`)
  - Rastreabilidade Empreendimento (`Grupo_COR0005_RastlddEmpnmnt`)
  - Percentual Energia Renovável Empreendimento (`PercEnerRenovvlEmpnmnt`)
  - Indicador Confirmação Alerta Sustentabilidade (`IndrConfcAlertSustldd`)
- Os campos suprimidos da mensageria são **mantidos** em telas e formulários
  RUCA – RUEC – CRUR conforme regras existentes.
- Escopo: processo batch de liberação de contratos + processos on-line RUCA
  que geram a COR0005 (Prorrogação, Manutenção Mestres Rurais, Produtividade
  Obtida e Desclassificação).

## Centros de custo impactados
- RUEC (montagem da COR0005 via RUEC8831)
- RUEC batch liberação (RUEC0895 — orquestrador, sem alteração prevista)

## Sequencia projeto (Capgemini — subida MF)

- Codigo: **2021-0765286-5-035**
- Uso: informar em pacotes Changeman, tickets de implantacao (TICKET-87532 / TICKET-87535) e solicitacao de implantacao PRD

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| **RUEC001652** | Implantacao AGROLEG-1239 (SICOR 5.12) | `RUEC8831`, `I#RUECC5` | Pacote criado | `pgm/ruec8831.cbl`, `cpy/i#ruecc5.cpy` |
| **RUEC001658** | Contingencia (versao PRD pre-1239) | `RUEC8831`, `I#RUECC5` | Pacote criado | baseline PRD pre-5.12 (certif/rastreab/energia ativos no layout) |

- **Implantacao:** pacote **RUEC001652** — exclusao COR0005 5.12 (`IndrConfcAlertSustldd`, certificacao, rastreabilidade, energia renovavel).
- **Contingencia:** pacote **RUEC001658** com fontes **baseline PRD** pre-demanda — layout COR0005 pre-5.12 para retorno pos-implantacao (`IndrConfcAlertSustldd`, certificacao, rastreabilidade e energia renovavel ativos na mensageria).

## Promocao ambientes (Jira)

| Ticket | Link | Pacote implantacao | Pacote contingencia | Status |
|--------|------|--------------------|---------------------|--------|
| **TICKET-87532** | https://jira.bradesco.com.br:8443/browse/TICKET-87532 | RUEC001652 | — | Criado |
| **TICKET-87535** | https://jira.bradesco.com.br:8443/browse/TICKET-87535 | — | RUEC001658 | Criado |

- Tickets Jira para implantacao dos pacotes Changeman em PRD (sequencia projeto **2021-0765286-5-035**).

## Escopo tecnico
- COBOL
- DB2
- BATCH
- CICS

## Fontes impactados (inicial)
- RUEC/pgm/ruec8831.cbl — montador da COR0005 (exclusão dos 4 campos — CAPG05)
- RUEC/cpy/i#ruecc5.cpy — layout da COR0005 (exclusão dos 4 campos — CAPG05)
- RUEC/pgm/ruec0895.cbl — batch liberação (sem alteração prevista)
- Demais fontes a levantar via subtarefas: AGROLEG-1392, 1395, 1396, 1400, 1402,
  1403, 1405, 1412, 1418, 1424, 1446, 1595

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - CMRUWPPR -> LIBS/COPYLIB/CMRU/cmruwppr.cpy
  - I#BRAD7C -> LIBS/COPYLIB/BRAD/I#BRAD7C.cpy
  - I#CKRS01 -> LIBS/COPYLIB/CKRS/i#ckrs01.cpy
  - I#RCOR10 -> LIBS/COPYLIB/RCOR/i#rcor10.cpy
  - I#RUECC5 -> Projetos/Capgemini/AGROLEG-1239/cpy/i#ruecc5.cpy
  - RUECW824 -> LIBS/COPYLIB/RUEC/ruecw824.cpy
  - RUECWGEO -> LIBS/COPYLIB/RUEC/ruecwgeo.cpy
- DCLGEN (encontradas):
  - RUCAB003 -> LIBS/DCLGEN/RUCA/rucab003.cpy
  - RUCAB004 -> LIBS/DCLGEN/RUCA/rucab004.cpy
  - RUECB040 -> LIBS/DCLGEN/RUEC/ruecb040.cpy
  - RUECB099 -> LIBS/DCLGEN/RUEC/ruecb099.cpy
  - RUECB09A -> LIBS/DCLGEN/RUEC/ruecb09a.cpy
  - RUECB09B -> LIBS/DCLGEN/RUEC/ruecb09b.cpy
  - RUECB09E -> LIBS/DCLGEN/RUEC/ruecb09e.cpy
  - RUECB09N -> LIBS/DCLGEN/RUEC/ruecb09n.cpy
  - RUECV012 -> LIBS/DCLGEN/RUEC/ruecv012.cpy
  - RUFIB002 -> LIBS/DCLGEN/RUFI/rufib002.cpy
  - RUFIB044 -> LIBS/DCLGEN/RUFI/rufib044.cpy
  - RUFIB092 -> LIBS/DCLGEN/RURC/rufib092.cpy
  - RUMOV002 -> Projetos/Capgemini/AGROLEG-1239/cpy/rumov002.cpy
  - RURCB000 -> LIBS/DCLGEN/RURC/rurcb000.cpy
  - RURCB00P -> LIBS/DCLGEN/RURC/rurcb00p.cpy
  - RURCB028 -> LIBS/DCLGEN/RURC/rurcb028.cpy
  - RURCB094 -> LIBS/DCLGEN/RURC/rurcb094.cpy
  - RURCB097 -> LIBS/DCLGEN/RURC/rurcb097.cpy
  - SQLCA -> LIBS/DCLGEN/sqlca.cpy
- Pendencias (nao encontradas em LIBS):
  - I#RUEC65
- Dependencias de sistema:
  - Nenhuma

## Dados Jira (sync)
- Chave: AGROLEG-1239
- Titulo: [Flash - RURA] - SICOR 512 - Liberação de contratos (COR0005 processo batch)
- Status: Em Desenvolvimento
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Fri, 22 May 2026 19:34:17 -0300

## Evidencias
- Pacote Changeman **RUEC001652** — implantacao `RUEC8831` + `I#RUECC5` (exclusao COR0005 SICOR 5.12)
- Pacote contingencia **RUEC001658** — versao PRD baseline pre-5.12
- Ticket implantacao **[TICKET-87532](https://jira.bradesco.com.br:8443/browse/TICKET-87532)** — pacote RUEC001652
- Ticket implantacao contingencia **[TICKET-87535](https://jira.bradesco.com.br:8443/browse/TICKET-87535)** — pacote RUEC001658
- Sequencia projeto Capgemini: **2021-0765286-5-035**

## Status
- [x] Em andamento
- [x] Em teste
- [x] Pronto para subida
- [x] Em homologacao
- [x] Implantado
