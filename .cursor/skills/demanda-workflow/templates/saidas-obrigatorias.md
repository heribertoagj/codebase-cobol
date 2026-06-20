# Saidas obrigatorias por fase

Substituir `AGROLEG-nnn` pelo ID da demanda (`STEF-nnn`, `7COMM-nnn`, etc.).

## Fase 1 — Inicializacao

```text
=== AGROLEG-nnn — INICIALIZACAO ===

NEGOCIO
- Objetivo:
- Iniciativa:
- Mensagens COR/CIR:

IMPACTO PRELIMINAR (acervo)
- Historias relacionadas:
- Suspeitos de impacto:

FONTES A BAIXAR
| Prioridade | Tipo | Member | Motivo | Destino |
| P1 | PGM/CPY/JCL/MFS | ... | ... | pgm/ cpy/ jcl/ mfs/ |

PROXIMO PASSO
Baixar fontes P1 e informar: "Fontes baixados — continuar analise AGROLEG-nnn"
```

## Fase 2 — Analise parcial

```text
=== AGROLEG-nnn — ANALISE PARCIAL ===

FONTES ANALISADAS
- (lista)

FONTES FALTANTES
| Tipo | Member | Motivo (COPY/CALL/referencia) | Prioridade |

PROXIMO PASSO
Baixar fontes faltantes e informar: "Fontes baixados — continuar analise AGROLEG-nnn"
```

## Fase 2 — Analise concluida

```text
=== AGROLEG-nnn — ANALISE CONCLUIDA ===

ESCOPO CONFIRMADO
- Programas:
- Copybooks:
- JCL/MFS:
- Malha batch (se aplicavel): link HTML em `## Malha batch`
- Tabelas DCLGEN:

RISCOS
- ...

PROXIMO PASSO
Informar: "Gerar plano de ajuste AGROLEG-nnn"
```

## Fase 3 — Plano de ajuste

```text
=== AGROLEG-nnn — PLANO DE AJUSTE ===

(resumo do plano)

GATE
Revise o plano no README. Para implementar, responda:
"Implementar plano AGROLEG-nnn"
```

## Fase 4 — Implementacao

```text
=== AGROLEG-nnn — IMPLEMENTACAO CONCLUIDA ===

FONTES ALTERADOS
- ...

PROXIMO PASSO
Informar: "Certificar riscos AGROLEG-nnn"
```

## Fase 4b — Certificacao de riscos

```text
=== AGROLEG-nnn — CERTIFICACAO DE RISCOS (CODIFICACAO) ===

RESUMO: [APROVADO | APROVADO COM RESSALVAS | REPROVADO]

| # | Risco (README) | Veredito | Evidencia |
|---|----------------|----------|-----------|
| 1 | ... | NAO REALIZADO | pgm/... §... |
| 2 | ... | PENDENTE TESTE | homologacao — Fase 6 |

RISCOS REALIZADOS (bloqueantes)
- (nenhum | lista)

PROXIMO PASSO
Informar: "Pre-subida AGROLEG-nnn"
```

## Fase 5b — Subida MF

```text
=== AGROLEG-nnn — SUBIDA MF ===

PACOTES CHANGEMAN
- Subida:       RUCA000xxx — members — compilado OK
- Contingencia: RUCA000yyy — baseline PRD

PROMOCAO AMBIENTES (JIRA)
- Ticket: TICKET-xxxxx — https://jira.bradesco.com.br:8443/browse/TICKET-xxxxx

PROXIMO PASSO
Homologacao funcional (Fase 6) — gestor testa **somente apos** subida MF
```

## Fase 7 — Encerramento

```text
=== AGROLEG-nnn — ENCERRAMENTO ===

ACERVO ATUALIZADO
- Modulos:
- Iniciativa:
- Indice regenerado

DEMANDA ENCERRADA
```
