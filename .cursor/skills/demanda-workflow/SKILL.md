---
name: demanda-workflow
description: Orquestra o ciclo ponta a ponta de demandas COBOL — Capgemini (AGROLEG via XML Jira), Stefanini (STEF via reuniao) e 7Comm (7COMM via reuniao). Use para Implementar AGROLEG-nnn.xml, Iniciar demanda Stefanini/7Comm, Clarificar requisitos, continuar analise, implementar plano, certificar riscos, pre-subida ou finalizar.
---

# Demanda Workflow

Orquestrador do ciclo completo, com **gates humanos** entre analise, plano, implementacao e encerramento.

Principios KISS e padroes do cliente (obrigatorios): [../README.md](../README.md)

## Canais de entrada

| Consultoria | ID | Fase 0 / 1 | Skill |
|---|---|---|---|
| Capgemini | `AGROLEG-nnn` | `Implementar AGROLEG-nnn.xml` | `jira-xml-readme-sync` |
| Stefanini | `STEF-nnn` | `Iniciar demanda Stefanini` | `demanda-intake-reuniao` |
| 7Comm | `7COMM-nnn` | `Iniciar demanda 7Comm` | `demanda-intake-reuniao` |

Fases 2–7 usam o **ID da demanda** nos comandos.

## Regra de ouro (padrao cliente — nao simplificar)

| Comando do usuario | Pode alterar fontes (.cbl/.cpy/.mfs/.jcl)? |
|---|---|
| `Iniciar demanda Stefanini` / `Iniciar demanda 7Comm` | **NAO** |
| `Implementar AGROLEG-nnn.xml` | **NAO** |
| `Fontes baixados — continuar analise <ID>` | **NAO** |
| `Gerar plano de ajuste <ID>` | **NAO** |
| `Clarificar requisitos <ID>` | **NAO** (so README / intake) |
| `Implementar plano <ID>` | **SIM** (apos validacao explicita) |
| `Certificar riscos <ID>` | **NAO** (so README — evidencia nos fontes) |
| `Pre-subida <ID>` | Correcoes bloqueantes apenas |
| `Finalizar <ID>` | README/acervo apenas |

**Nunca** implementar codigo na Fase 1 ou 2. **Nunca** encerrar acervo antes da homologacao implantada em PRD.

## Mapa de fases

| Fase | Nome | Gate | Detalhe |
|---|---|---|---|
| 1 | Inicializacao + impacto preliminar | Usuario baixa fontes | [phases/01-inicializacao.md](phases/01-inicializacao.md) |
| 2 | Analise iterativa de fontes | Analise concluida | [phases/02-analise.md](phases/02-analise.md) |
| 3 | Plano de ajuste | Usuario valida plano | [phases/03-plano.md](phases/03-plano.md) |
| 4 | Desenvolvimento | Certificacao de riscos | [phases/04-desenvolvimento.md](phases/04-desenvolvimento.md) |
| 4b | Certificacao de riscos (pos-desenvolvimento) | Pre-subida | [phases/04b-certificacao-riscos.md](phases/04b-certificacao-riscos.md) |
| 5 | Pre-subida | Usuario sobe MF | [phases/05-pre-subida.md](phases/05-pre-subida.md) |
| 5b | Subida MF (Changeman) | Compile OK | [reference/subida-mf-changeman.md](reference/subida-mf-changeman.md) |
| 6 | Homologacao (gestor) | Usuario implanta PRD | [phases/06-homologacao.md](phases/06-homologacao.md) |
| 7 | Encerramento | — | [phases/07-encerramento.md](phases/07-encerramento.md) |

**Ao executar uma fase:** ler somente o arquivo `phases/` ou `reference/` correspondente + [templates/saidas-obrigatorias.md](templates/saidas-obrigatorias.md).

## Comandos do usuario

| Momento | Comando |
|---|---|
| Inicio Stefanini/7Comm | `Iniciar demanda Stefanini` / `Iniciar demanda 7Comm` |
| Inicio Capgemini | `Implementar AGROLEG-nnn.xml` |
| Apos baixar fontes | `Fontes baixados — continuar analise <ID>` |
| README com lacunas de negocio | `Clarificar requisitos <ID>` |
| Solicitar plano | `Gerar plano de ajuste <ID>` |
| Apos validar plano | `Implementar plano <ID>` |
| Apos desenvolvimento | `Certificar riscos <ID>` |
| Apos certificacao de riscos | `Pre-subida <ID>` |
| Pos-implantacao | `Finalizar <ID>` |

Legado: `Iniciar AGROLEG-nnn`, `Analisar impacto AGROLEG-nnn` (equivalem a Fase 1).

## Skills filhas por tipo

| Tipo | Skills / referencias |
|---|---|
| Qualquer COBOL | `bradesco-style`, `bradesco-quality-code` |
| SICOR 5.12 | `agroleg-knowledge/initiatives/SICOR-5.12.md` |
| CICS/BMS | `cics-bms-maps` |
| SQL/DB2 | rule `dclgen-sql-validation`, `readme-dependencias-libs` |
| BATCH / JCL | `malha-batch-analise`, `referencia-batch-malha/` |
| Conhecimento | `agroleg-knowledge` |
| Pre-subida | `demanda-pre-subida` |
| README | `readme-demanda-template`, `jira-xml-readme-sync`, `demanda-intake-reuniao` |
| Requisitos vagos / ambiguidade | `brainstorming`, `clarification` |

## Estrutura de pastas

Ver [reference/estrutura-pastas.md](reference/estrutura-pastas.md).
