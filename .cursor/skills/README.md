# Skills AGROLEG — principios de autoracao

## KISS (Keep It Simple, Stupid)

Aplicar KISS na **estrutura das skills**, nao nos padroes do cliente.

| Fazer (KISS) | Nao fazer |
|---|---|
| SKILL.md como roteador; detalhe em `phases/`, `reference/`, `templates/` | Duplicar regras COBOL em varias skills |
| Uma fonte da verdade por tema | Repetir checklist Hexavision fora de `referencia-cobol/` |
| Acionar skill filha em vez de copiar passos | Criar comandos ou fases extras sem necessidade |
| Conhecimento de negocio mutavel em `agroleg-knowledge/initiatives/` | Skill permanente para epic/regulatorio |
| Script existente → "rodar script X" | Reexplicar o que o script ja faz |

**Meta:** `demanda-workflow/SKILL.md` permanece enxuto (~120 linhas); o agente le `phases/<NN>-*.md` somente na fase ativa.

---

## Nao simplificar — padroes do cliente (obrigatorios)

Os itens abaixo **nao sao candidatos a KISS**. Sao exigencias do cliente Bradesco/Capgemini e devem permanecer intactos em qualquer refatoracao de skills:

1. **Gates humanos** entre analise, plano, implementacao, certificacao de riscos (Fase 4b) e encerramento — o agente nao avanca de fase sem comando explicito do usuario.
2. **Rastreio de alteracao COBOL** — comentar linha antiga + criar linha nova; verbos comentados com `-`; **nunca excluir linhas** (`referencia-cobol/03-rastreio-alteracao.md`, `bradesco-quality-code`).
3. **Separacao do acervo** — iniciativa (`initiatives/`) vs modulo (`modules/`) vs historia (README da demanda).
4. **Certificacao de riscos pos-desenvolvimento (Fase 4b)** — verificar nos fontes que riscos de codificacao do README nao se materializaram; preencher `## Certificacao de riscos (pos-desenvolvimento)` antes da pre-subida.
5. **Gate unificado de pre-subida** — `demanda-pre-subida` consolida Hexavision, SICOR 5.12, DCLGEN/SQL, LIBS, certificacao de riscos e checklist do README antes de subida MF.
6. **Homologacao pos-subida** — gestor testa somente apos compile Changeman; encerramento de acervo somente apos implantacao PRD confirmada.
7. **Subida MF (Changeman)** — pacote de subida, pacote de contingencia (baseline PRD), ticket Jira de promocao e sequencia projeto Capgemini (`reference/subida-mf-changeman.md`).

Refatorar skills para reduzir tokens **sem** enfraquecer, omitir ou fundir esses controles.

---

## Mapa de skills

| Skill | Papel |
|---|---|
| `demanda-workflow` | Orquestrador do ciclo (Fases 1–7 + 4b certificacao de riscos) |
| `demanda-intake-reuniao` | Entrada Stefanini / 7Comm |
| `jira-xml-readme-sync` | Entrada Capgemini (XML Jira) |
| `agroleg-knowledge` | Acervo — consulta e captura |
| `bradesco-style` | Padrao visual COBOL |
| `bradesco-quality-code` | Gate Hexavision (indice → `referencia-cobol/`) |
| `demanda-pre-subida` | Gate unificado pre-subida |
| `cics-bms-maps` | CICS / BMS |
| `malha-batch-analise` | Malha batch / JCL |
| `readme-dependencias-libs` | COPY / DCLGEN vs LIBS |
| `readme-demanda-template` | Template README |
| `brainstorming` | Requisitos vagos — protocolo socratico (pre-intake / Fase 1) |
| `clarification` | Ambiguidades no README — ate 5 perguntas (`Clarificar requisitos <ID>`) |

Regras COBOL completas: `Cobol/Projetos/_indice/referencia-cobol/`

Todas as skills do projeto ficam em `.cursor/skills/`.
