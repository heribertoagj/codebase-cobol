# AGROLEG-1243 - SICOR 512 - COR0001E / compactacao WKTPart na formalizacao

## Status workflow
- Fase atual: **Encerrada** — implantado PRD
- Ultimo comando: `Finalizar AGROLEG-1243`
- Proximo passo: N/A

## Objetivo
- Como PO, alterar na formalizacao da proposta o processo de montagem da mensagem de erro **COR0001E** no retorno da COR0001 para nao gravar espacos em branco no campo de coordenada geodesica (**WKTPart**, tag `{2210}`).
- Garantir que a COR0001E retornada do BACEN seja persistida/exibida sem espacos desnecessarios, sem ultrapassar o tamanho da tabela, mesmo com contratos com mais de tres glebas.

## Restricao de escopo (workspace RCOR)

Esta pasta contem a implementacao no centro de custo **RCOR** (recepcao MQ, parse SIMM, persistencia COR0001E em DB2).

| Incluido nesta pasta | Fora desta pasta (Jira / subtarefas) |
|----------------------|--------------------------------------|
| Compactacao WKTPart `{2210}` na COR0001E recebida/persistida | Geracao COR0001 na formalizacao em **RUEC**, **RUCA**, **CRUR** |
| RCOR5050, RCOR6006, RCOR6007, RCOR6010, RCOR8010, RCOR8015, RCORB001, RCORB002, TMNF5026 | Subtarefas Jira AGROLEG-1324 … AGROLEG-1337 |

- Validacao funcional integrada: formalizar em RUEC/CRUR/RUCA com erro COR0001E e consultar WKTPart no RCOR (query ou tela).

## Requisito de negocio
- Iniciativa: SICOR 5.12 — ver `.cursor/skills/agroleg-knowledge/initiatives/SICOR-5.12.md`
- Mensagens COR/CIR desta historia: **COR0001E** (retorno de erro da COR0001)
- Regra de negocio resumida:
  1. Contratos com mais de 3 glebas geram COR0001E unica que pode exceder limite de tabela por espacos no WKTPart.
  2. Eliminar espacos em branco desnecessarios no WKTPart antes de persistir/atualizar.
  3. Manter exibicao correta do campo apos formalizacao.

## Centros de custo impactados
- **RCOR** (implementacao nesta pasta)
- **RUEC, RUCA, CRUR** (geracao COR0001 na formalizacao — escopo Jira; ver subtarefas)

## Escopo tecnico
- COBOL: compactacao de segmentos `{2210}` / WKTPart em buffer COR0001E (ate 30000 bytes MSG; limite 10000 bytes por ocorrencia `{2210}`)
- CICS/IMS: camada RCOR8015 (negocio recepcao COR0001E), RCOR8010, RCOR6007 (varredura buffer MQ)
- DB2: RCORB001, RCORB002 (persistencia mensagem particionada)
- MQ: RCOR6006 / SBAT3510 (fluxo legado mantido)
- BATCH: TMNF5026 (contexto TMNF — dependencias DCLGEN pendentes em LIBS)

## Malha batch
- N/A para escopo principal (online/MQ + DB2)

## Fontes impactados (preliminar)
- Programa(s): RCOR8015, RCOR6007, RCORB002 (alterados); RCOR8010 (documentado); RCORB001 (rastreio); RCOR5050, RCOR6006, RCOR6010, TMNF5026 (cadeia/contexto)
- Copybook(s): I#RCOR09, I#RCOR13, I#RCOR16, I#SIMM03, I#SIMM04, I#POOLB6, I#SBATMV (via LIBS)
- DCLGEN(s): RCORB000, RCORB001, RCORB002, SQLCA
- JCL(s): N/A
- Mapa(s) BMS: N/A

## Estrutura de pastas (componentes)

```text
AGROLEG-1243/
├── README.md
├── AGROLEG-1243.xml
└── pgm/          programas RCOR/TMNF (.cbl)
```

Ver `Cobol/Projetos/_indice/guia-organizacao-seguranca.md`.

## Fontes pendentes baixa
| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | PGM | RCOR8015 | compactacao WKTPart COR0001E | pgm/ | baixado |
| P1 | PGM | RCOR6007 | normaliza buffer MQ | pgm/ | baixado |
| P1 | PGM | RCORB002 | espelho logica 8015 / snap WKT | pgm/ | baixado |
| P1 | PGM | RCOR8010 | orquestracao recepcao MQ | pgm/ | baixado |
| P1 | PGM | RCORB001 | persistencia RCORB002 | pgm/ | baixado |
| P2 | PGM | RCOR5050, RCOR6006, RCOR6010, TMNF5026 | cadeia RCOR/TMNF | pgm/ | baixado |
| P2 | DCLGEN | TMNF* (ver pendencias LIBS) | TMNF5026 | — | pendente em LIBS |
| P1 | — | RUEC/RUCA/CRUR (geracao COR0001) | escopo Jira fora RCOR | outras subtarefas | pendente |

## Impacto tecnico (confirmado)
- Programas alterados (tags AG1243* / CG2605*):
  - **RCOR8015** — rotinas 1050–1053: busca tag `{2210}`, localiza fim segmento, colapsa espacos duplos, limite 10000 bytes/ocorrencia
  - **RCOR6007** — PERFORM 3000-NORMALIZA-WKT-WKTPART no buffer MQ completo (SIMM03/SIMM04+COR)
  - **RCORB002** — logica espelhada de compactacao + snap WKT gleba 1; buffer WRK-WKT-BUF X(30000)
  - **RCOR8010** — mantem fluxo MQ legado RCOR6006/SBAT3510; compactacao delegada ao RCOR8015
- Programas na cadeia sem alteracao de codigo identificada ainda: RCOR5050, RCOR6006, RCOR6010, TMNF5026
- Copybooks alterados: nenhum local (somente LIBS)
- Tabelas DB2 (via DCLGEN): RCORB001 (`DB2PRD.TPARM_SIST_CREDT`), RCORB002 (sequencia mensagem particionada)
- Fluxo resumido: BACEN retorna COR0001E → MQ/SBAT → RCOR6007/8010/8015 parse SIMM → compacta WKTPart `{2210}` → RCORB002/6010 persiste → consulta RCOR/TMNF

## Plano de ajuste (proposta)
| # | Fonte | Area/Section | Alteracao proposta | Referencia |
|---|---|---|---|---|
| 1 | RCOR8015 | 1050–1053 | Compactar espacos WKTPart `{2210}` pre-DB2 | Jira + cap. 03 |
| 2 | RCOR6007 | 3000-NORMALIZA-WKT-WKTPART | Varredura buffer MQ COR0001E | Jira |
| 3 | RCORB002 | rotinas AG1243* | Espelhar compactacao + diagnostico WKT | Jira + cap. 03 |
| 4 | RCOR8010 | header/fluxo | Documentar delegacao compactacao ao 8015 | cap. 03 |

### Ordem de implementacao
1. RCOR8015 (nucleo compactacao)
2. RCOR6007 (normalizacao pre-parse)
3. RCORB002 (persistencia alinhada)
4. Revisao RCOR8010 / regressao cadeia MQ

### Riscos do plano
- Alteracao de formato COR0001E pelo BACEN
- Mensagem ainda exceder limite apos compactacao (muitas glebas)
- DCLGEN TMNF pendentes em LIBS dificultam analise TMNF5026

### Cenarios de teste (preliminar)
- Incluir/alterar proposta RUEC/CRUR com >4 glebas invalidas → COR0001E → validar WKTPart sem espacos (RCOR/query)
- Alterar contrato RUCA op.33 → COR0001E → mesma validacao
- Regressivo: proposta sem glebas → COR0001E → campos corretos

## Decisoes tecnicas
- Prefixo rastreio **AG1243*** (7Comm) nos fontes RCOR8015/RCORB002; **CG2605*** em RCOR6007/RCOR8010/RCORB002 (Capgemini)
- Buffer mensagem COR0001E: X(30000); limite por ocorrencia `{2210}`: 10000 bytes
- Linhas comentadas preservadas (cap. 03); verbos inviaveis com `-` onde aplicavel

## Historias relacionadas
| Historia | Relacao | Observacao |
|---|---|---|
| AGROLEG-1482 | complementa | COR0001 layout SICOR 5.12 RUCA — escopo distinto |
| AGROLEG-1109 | mesma-iniciativa | Levantamento SICOR 5.12 / COR0001 |
| AGROLEG-1324 … 1337 | subtarefa | Subtarefas Jira desta historia |

## Licoes para proximas analises
- Compactacao WKTPart deve ocorrer **antes** da persistencia RCORB002 e no buffer MQ (6007) para evitar truncamento DB2
- Reutilizar rotinas 1050–1053 do RCOR8015 como referencia para RCORB002

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - I#BRAD7C -> Cobol/LIBS/COPYLIB/BRAD/I#BRAD7C.cpy
  - I#POOLB6 -> Cobol/LIBS/COPYLIB/POOL/i#poolb6.cpy
  - I#RCOR03 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor03.cpy
  - I#RCOR09 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor09.cpy
  - I#RCOR11 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor11.cpy
  - I#RCOR12 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor12.cpy
  - I#RCOR13 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor13.cpy
  - I#RCOR15 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor15.cpy
  - I#RCOR16 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor16.cpy
  - I#RCOR16 -> Cobol/LIBS/DCLGEN/RCOR/i#rcor16.cpy
  - I#RCOR17 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor17.cpy
  - I#RCOR17 -> Cobol/LIBS/DCLGEN/RCOR/i#rcor17.cpy
  - I#RCOR18 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor18.cpy
  - I#RCOR18 -> Cobol/LIBS/DCLGEN/RCOR/i#rcor18.cpy
  - I#RCOR31 -> Cobol/LIBS/COPYLIB/RCOR/i#rcor31.cpy
  - I#RCOR31 -> Cobol/LIBS/DCLGEN/RCOR/i#rcor31.cpy
  - I#RUEC68 -> Cobol/LIBS/COPYLIB/RUEC/i#ruec68.cpy
  - I#SBATMV -> Cobol/LIBS/COPYLIB/SBAT/i#sbatmv.cpy
  - I#SIMM03 -> Cobol/LIBS/COPYLIB/SIMM/i#simm03.cpy
  - I#SIMM04 -> Cobol/LIBS/COPYLIB/SIMM/i#simm04.cpy
  - SBATWC01 -> Cobol/LIBS/COPYLIB/SBAT/sbatwc01.cpy
- DCLGEN (encontradas):
  - RCORB000 -> Cobol/LIBS/DCLGEN/RCOR/rcorb000.cpy
  - RCORB001 -> Cobol/LIBS/DCLGEN/RCOR/rcorb001.cpy
  - RCORB002 -> Cobol/LIBS/DCLGEN/RCOR/rcorb002.cpy
  - SQLCA -> Cobol/LIBS/DCLGEN/sqlca.cpy
- Pendencias (nao encontradas em LIBS):
  - SPBNW504, SPBNWNP5
  - TMNFB009, TMNFB019, TMNFB031, TMNFB032, TMNFB034, TMNFB036, TMNFB053, TMNFB078
  - TMNFW0C0, TMNFW0C3, TMNFW0D0, TMNFW820, TMNFW997, TMNFW998
  - TMNFWE04, TMNFWE05, TMNFWE09, TMNFWE11, TMNFWE16, TMNFWO01
  - TMNFWS02, TMNFWS03, TMNFWS04, TMNFWS05
- Dependencias de sistema:
  - Nenhuma

## Riscos e atencoes
- Alteracao de formato COR0001E pelo BACEN
- Tamanho excedente mesmo apos compactacao
- Dependencia ambiente BACEN para testes (>4 glebas invalidas)
- Ajuste previo de layout de tabela para mensagens maiores (dependencia Jira)

## Plano de testes
- [x] Inclusao RUEC/CRUR com >4 glebas → COR0001E → WKTPart sem espacos
- [x] Alteracao RUEC/CRUR → COR0001E → validacao query RCOR
- [x] Alteracao RUCA op.33 → COR0001E → validacao query RCOR
- [x] Regressivo sem glebas → COR0001E → campos corretos
- [x] Validacao integrada pos-compactacao RCOR8015 + RCOR6007 + RCORB002

## Sequencia projeto (Capgemini — subida MF)

- Codigo: **2021-0765286-5-035**
- Uso: informar em pacotes Changeman, tickets de promocao e solicitacao de implantacao PRD

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| | Subida (implementacao) | RCOR8015, RCOR6007, RCORB002 (+ cadeia) | **Implantado PRD** | `pgm/` |
| | Contingencia (versao PRD pre-demanda) | RCOR* | Implantado PRD | baseline pre-1243 |

- Implantado PRD — sequencia projeto **2021-0765286-5-035** (numeros de pacote Changeman nao registrados no acervo)

## Promocao ambientes (Jira)

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| | | | | Implantado PRD |

## Evidencias
- Implantado PRD — compactacao WKTPart `{2210}` em RCOR8015, RCOR6007, RCORB002
- Sequencia projeto Capgemini: **2021-0765286-5-035**
- Homologacao gestor: OK

## Dados de origem
- Consultoria: Capgemini
- Canal: Jira XML (`AGROLEG-1243.xml`)
- Referencia externa: https://jira.bradesco.com.br:8443/browse/AGROLEG-1243
- Chave Jira (Capgemini): AGROLEG-1243
- Titulo: [Flash - CRUR] - SICOR 512 - Alterar processo de geracao COR0001E na formalizacao da proposta
- Status: Product Backlog
- Responsavel: HERIBERTO ANTONIO GIANNASI JUNIOR
- Atualizado em: Thu, 23 Apr 2026 16:19:10 -0300

## Status
- [x] Em andamento
- [x] Em teste (dev interno — pre-subida)
- [x] Pronto para subida
- [x] Em homologacao (gestor testa **apos** subida MF)
- [x] Implantado
