# Categorias de clarificacao → README AGROLEG

## Taxonomia (varredura)

| Categoria | O que verificar no README/intake |
|---|---|
| Escopo funcional | Objetivo, regra de negocio, fora de escopo |
| Dominio / dados | Tabelas DCLGEN, copybooks, campos novos |
| Fluxo / UX | Jornada CICS, transacao, mapa BMS |
| Nao-funcionais | Performance batch, FILE STATUS, volume |
| Integracoes | COPY, CALL, JCL, malha, APIs externas |
| Edge cases | Erros, mensagens, estados vazios |
| Constraints | SICOR 5.12, Hexavision, prazo MF |
| Terminologia | COR/CIR, nomes de programa, CC |
| Criterios de conclusao | Cenarios de teste, aceite do gestor |
| Pendencias | TODO, decisoes em aberto |

## Mapeamento categoria → secao README

| Categoria | Secao alvo | Acao |
|---|---|---|
| Escopo funcional | `## Objetivo`, `## Requisito de negocio` | Clarificar bullet ou adicionar |
| Atores / CC | `## Centros de custo impactados` | Listar ou corrigir |
| Dados / entidades | `## Escopo tecnico` (DB2), `## Impacto tecnico` | Campos, DCLGEN, tabelas |
| Batch / malha | `## Malha batch`, `## Escopo tecnico` (BATCH/JCL) | Job, dependencias |
| CICS | `## Escopo tecnico` (CICS), Fontes impactados | Transacao, mapa |
| SICOR | `## Requisito de negocio`, iniciativa | Mensagens, tags, catalogo |
| Edge cases / testes | `### Cenarios de teste (preliminar)` | Novo bullet |
| Decisoes | `## Decisoes tecnicas` | Uma entrada por resposta |
| Historico | `## Clarificacoes` (criar se ausente) | Q → A por sessao |

### Template `## Clarificacoes` (criar na primeira sessao)

```markdown
## Clarificacoes

| Data | Sessao | Pergunta | Resposta |
|------|--------|----------|----------|
| 2026-06-12 | 1 | ... | ... |
```

## Exemplos de perguntas (COBOL)

### P0 — Mensagem SICOR

**Pergunta:** Qual mensagem COR/CIR esta historia altera?

**Recomendado:** Confirmar codigo exato (ex. COR0005) antes de mapear copybooks.

### P0 — Escopo de programa

**Pergunta:** A regra aplica-se somente ao programa citado no Jira ou a toda a cadeia de CALL?

### P1 — Batch vs online

**Pergunta:** A validacao ocorre na tela (CICS) ou no job batch (JCL)?

### P1 — Criterio de aceite

**Pergunta:** O gestor valida com qual cenario minimo em homologacao?

## Criterios de inclusao / exclusao

**Incluir pergunta se:**

- Resposta impacta escopo de programas, plano de ajuste ou testes
- Reduz risco de rework na Fase 4

**Excluir se:**

- Melhor resolvida baixando fonte (Fase 2)
- Puramente estilistica ou ja coberta pelo XML Jira sem ambiguidade

## Encerramento

Parar quando:

- Ambiguidades criticas resolvidas
- Usuario diz "pronto" / "sem mais perguntas"
- 5 perguntas feitas
- Fila vazia

Se quota esgotada com itens deferred: listar no relatorio e sugerir nova sessao `Clarificar requisitos <ID>`.
