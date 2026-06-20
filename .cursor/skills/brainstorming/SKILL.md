---
name: brainstorming
description: Protocolo socratico para requisitos vagos ou complexos em demandas COBOL AGROLEG. Use antes do intake Stefanini/7Comm, quando notas de reuniao forem incompletas, ou quando o usuario descrever demanda sem escopo claro — antes de Implementar AGROLEG-nnn.xml ou Gerar plano de ajuste.
---

# Brainstorming (requisitos AGROLEG)

## Objetivo

Esclarecer requisitos **antes** de analise ou implementacao. Nao altera fontes COBOL — apenas README, `intake.md` ou respostas no chat.

## Quando acionar

| Situacao | Acao |
|---|---|
| Notas de reuniao vagas (`Iniciar demanda Stefanini/7Comm`) | Acionar **antes** de fechar intake |
| XML Jira com descricao incompleta | Acionar na Fase 1, antes de listar fontes P1 |
| Usuario pede feature sem programa/mensagem/CC | Parar e perguntar |
| Escopo ou regra de negocio ambigua | Acionar `clarification` se README ja existir |

## Neste projeto (AGROLEG / COBOL Bradesco)

- Respeitar gates do `demanda-workflow` — **nao implementar** sem `Implementar plano <ID>`.
- Artefatos: `README.md`, `intake.md` (nao `req.md` / `spec.md` separados).
- Stakeholders: analista, gestor de negocio, equipe Capgemini / Stefanini / 7Comm.
- Perguntas ao usuario: usar ferramenta `AskQuestion` quando disponivel; senao chat com opcoes em tabela.
- Contexto COBOL: ver [reference/agroleg-context.md](reference/agroleg-context.md).

## Gate socratico (obrigatorio)

**Nao iniciar analise profunda nem codigo** quando:

- Pedido do tipo "alterar programa X" sem regra de negocio
- Demanda SICOR sem mensagem COR/CIR identificada
- Escopo tecnico (COBOL/JCL/CICS/BATCH) indefinido

**Minimo antes de prosseguir:**

1. **Parar** — nao listar fontes nem editar `.cbl`
2. **Perguntar** — pelo menos 3 questoes (proposito, usuarios/ator, escopo must-have vs nice-to-have)
3. **Aguardar** resposta do usuario

## Formato de pergunta

```markdown
### [P0|P1|P2] **[PONTO DE DECISAO]**

**Pergunta:** ...

**Por que importa:** impacto em programa/copybook/malha/SICOR

**Opcoes:**
| Opcao | Pros | Contras |
| A | ... | ... |

**Se nao especificado:** [default + justificativa]
```

Principios e algoritmo completo: [reference/dynamic-questioning.md](reference/dynamic-questioning.md)

## Integracao com fluxo

| Fase | Uso |
|---|---|
| Pre-intake (STEF/7COMM) | `demanda-intake-reuniao` aciona este skill se notas vagas |
| Fase 1 | Apos sync Jira, se `## Objetivo` ou `## Requisito de negocio` insuficientes |
| Pos-brainstorming | Registrar em README ou `intake.md`; seguir `demanda-workflow` |

Se README ja existir com lacunas estruturadas, preferir skill `clarification` (`Clarificar requisitos <ID>`).

## Anti-patterns

- Implementar antes de entender regra de negocio
- Assumir programa impactado sem confirmar
- Pular gate humano do workflow
