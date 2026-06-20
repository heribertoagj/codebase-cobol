# Catalogo de Demandas

## Convencoes
- Capgemini (Jira): `Projetos/Capgemini/AGROLEG-nnn/` ou `Finalizada/AGROLEG-nnn/`
- Stefanini (reuniao): `Projetos/Stefanini/STEF-nnn/`
- 7Comm (reuniao): `Projetos/7Comm/7COMM-nnn/`
- README padrao: `Projetos/_indice/template-README-demanda.md`
- Intake reuniao: `Projetos/_indice/template-intake-reuniao.md`
- Guia de fluxo: `Projetos/_indice/guia-fluxo-agroleg.md`
- Indice de conhecimento: `.cursor/skills/agroleg-knowledge/index.md`
- Nao registrar senhas. Usar cofre.

## Fluxo por consultoria

| Consultoria | Entrada | Fase 0 / 1 | Skill intake |
|---|---|---|---|
| Capgemini | XML Jira | `Implementar AGROLEG-nnn.xml` | — |
| Stefanini | Reuniao | `Iniciar demanda Stefanini` | `demanda-intake-reuniao` |
| 7Comm | Reuniao | `Iniciar demanda 7Comm` | `demanda-intake-reuniao` |

Fases 2–7: mesmos comandos com ID da demanda (`STEF-nnn`, `7COMM-nnn` ou `AGROLEG-nnn`).

## Fluxo AGROLEG / demandas (7 fases + 4b)

| Fase | Acao | Comando |
|---|---|---|
| 1 | Inicializacao + impacto preliminar | `Implementar AGROLEG-nnn.xml` |
| 2 | Analise iterativa de fontes | `Fontes baixados — continuar analise AGROLEG-nnn` |
| 3 | Plano de ajuste | `Gerar plano de ajuste AGROLEG-nnn` |
| 4 | Desenvolvimento | `Implementar plano AGROLEG-nnn` |
| 4b | Certificacao de riscos (pos-desenvolvimento) | `Certificar riscos AGROLEG-nnn` |
| 5 | Pre-subida | `Pre-subida AGROLEG-nnn` |
| 6 | Homologacao | (humano) |
| 7 | Encerramento | `Finalizar AGROLEG-nnn` |

Skill orquestradora: `demanda-workflow`
Referencia COBOL (dev + gate): `Projetos/_indice/referencia-cobol/`

## Demandas (preencher e manter)

| Demanda      | Tipo      | Centro de Custo | Status           | Caminho |
|--------------|-----------|-----------------|------------------|---------|
| AGROLEG-993  | Alocacao  | RUCA, RUEC      | Finalizada       | Projetos/Capgemini/Finalizada/AGROLEG-993 |
| AGROLEG-1239 | Alocacao  | RUEC            | Implantacao PRD — RUEC001652 | Projetos/Capgemini/AGROLEG-1239 |
| AGROLEG-1243 | Alocacao  | RCOR            | Finalizada       | Projetos/Capgemini/Finalizada/AGROLEG-1243 |
| AGROLEG-1482 | Alocacao  | RUCA            | Implementacao concluida — pre-subida | Projetos/Capgemini/AGROLEG-1482 |
| AGROLEG-1109 | Alocacao  | RUEC, RUCA, RCOR, CRUR | Finalizada | Projetos/Capgemini/Finalizada/AGROLEG-1109 |
| STEF-001     | Projeto   | GFCT            | Em andamento     | Projetos/Stefanini/STEF-001 |
| STEF-002     | Projeto   | CEPT            | Inicializacao    | Projetos/Stefanini/STEF-002 |
| 7COMM-001    | Projeto   | SLIG            | Analise em andamento | Projetos/7Comm/7COMM-001 |
| 7COMM-002    | Projeto   | RUCA            | Intake — aguardando fontes | Projetos/7Comm/7COMM-002 |
| 7COMM-003    | Projeto   | SLIG            | Plano proposto — aguardando validacao | Projetos/7Comm/7COMM-003 |

## Checklist por fase

### Fase 1–2 (analise)
- [ ] XML Jira sincronizado
- [ ] Fontes pendentes baixa atualizada
- [ ] Malha batch JSON + HTML (se escopo BATCH)
- [ ] Analise concluida declarada pelo agente

### Fase 3–4 (plano + dev)
- [ ] Plano de ajuste validado pelo usuario (referencia cap. NN no README)
- [ ] Implementacao conforme plano e `referencia-cobol/` (caps. 01–09)
- [ ] Alteracoes: comentar + nova linha (cap. 03)

### Fase 5–6 (subida + homologacao)
- [ ] Pre-subida aprovada
- [ ] Evidencias registradas
- [ ] Implantado em producao

### Fase 7 (encerramento)
- [ ] Impacto confirmado + licoes
- [ ] Acervo atualizado (modules + initiatives)
- [ ] Indice regenerado
