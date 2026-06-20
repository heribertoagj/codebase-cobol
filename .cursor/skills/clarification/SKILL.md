---
name: clarification
description: Varredura estruturada de ambiguidades no README ou intake de demanda COBOL AGROLEG. Gera ate 5 questoes prioritizadas e integra respostas no README. Use com Clarificar requisitos AGROLEG-nnn, STEF-nnn ou 7COMM-nnn, antes do plano de ajuste ou quando analise nao puder concluir por lacunas de negocio.
---

# Clarification (requisitos AGROLEG)

## Objetivo

Reduzir ambiguidade em `README.md` (e `intake.md` se STEF/7COMM) **sem alterar fontes COBOL**.

## Comando do usuario

```text
Clarificar requisitos <ID>
```

Exemplos: `Clarificar requisitos AGROLEG-1485`, `Clarificar requisitos STEF-001`

## Quando usar

| Momento | Condicao |
|---|---|
| Apos Fase 1 | `## Objetivo` ou `## Requisito de negocio` incompletos |
| Durante Fase 2 | Analise bloqueada por **lacuna de negocio** (nao por fonte faltante) |
| Antes da Fase 3 | Usuario quer refinar README antes do plano |

**Nao usar** quando especificacao ja esta clara ou quando falta apenas baixar fontes.

## Fluxo

1. Carregar `Cobol/Projetos/<Consultoria>/<ID>/README.md` (+ `intake.md` se existir).
2. Varredura nas categorias — ver [reference/categorias-readme.md](reference/categorias-readme.md).
3. Gerar ate **5** questoes (impacto x incerteza).
4. Apresentar **uma questao por vez**; usar `AskQuestion` quando disponivel.
5. Apos cada resposta valida: integrar no README (secoes mapeadas em `categorias-readme.md`).
6. Registrar decisoes em `## Decisoes tecnicas` e, se primeira rodada, tabela em `## Clarificacoes` (criar se ausente).
7. Relatorio final com cobertura e proximo passo do `demanda-workflow`.

## Formato de pergunta

```markdown
### [P0|P1] **[PONTO DE DECISAO]**

**Pergunta:** ...

**Recomendado:** Opcao A — [justificativa tecnica COBOL/negocio]

| Opcao | Descricao |
| A | ... |
| B | ... |

Responda com a letra, "sim"/"recomendado", ou resposta curta (≤5 palavras).
```

## Regras

- Maximo 5 perguntas por sessao; retry na mesma pergunta nao conta como nova.
- Nao alterar `.cbl`, `.cpy`, `.jcl`, `.mfs`.
- Respeitar gates do `demanda-workflow` — clarificacao **nao substitui** `Implementar plano <ID>`.
- Se arquivo nao existir: orientar `Implementar AGROLEG-nnn.xml` ou intake STEF/7COMM primeiro.

## Saida final

```text
=== <ID> — CLARIFICACAO ===

PERGUNTAS: N/5 respondidas
ARQUIVO: README.md (+ intake.md se aplicavel)
SECOES ATUALIZADAS: ...

COBERTURA
- Clear: ...
- Resolvido: ...
- Deferred: ...

PROXIMO PASSO
[Fontes baixados — continuar analise <ID> | Gerar plano de ajuste <ID>]
```

## Integracao

- Requisito vago inicial: preferir `brainstorming` antes de existir README.
- Mapeamento categoria → secao README: [reference/categorias-readme.md](reference/categorias-readme.md)
- Pipeline: `demanda-workflow` Fases 1–3
