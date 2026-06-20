# 10 — Checklist gate (pre-subida)

Checklist unificado para **Fase 5** (`Pre-subida AGROLEG-nnn`) e revisao Hexavision.

Referencia completa: `Projetos/_indice/referencia-cobol/README.md`

## Formato de relatorio

```text
=== PRE-SUBIDA: [programa] ===

RESUMO: [APROVADO | APROVADO COM RESSALVAS | REPROVADO]

[secao]: [OK | BLOQUEANTE | AVISO | N/A] — observacao
```

## Formato de achado

```text
BLOQUEANTE: [cap. NN / regra]
Trecho encontrado: [comando ou situacao]
Risco: [truncamento, Hexavision, parse SICOR, etc.]
Correcao recomendada: [acao objetiva]
```

---

## 01 — Estrutura

- [ ] Cabecalho Bradesco (programa alterado)
- [ ] Toda logica em SECTION com `-99-FIM. EXIT.`
- [ ] GO TO restrito a mesma SECTION
- [ ] Programa novo: 1000/2000/3000 quando aplicavel
- [ ] Indentacao col 20/40 em trechos novos

## 02 — Comentarios / molduras

- [ ] Molduras consistentes em trechos alterados
- [ ] Sem mistura de estilos de comentario

## 03 — Rastreio de alteracao

- [ ] **Nenhuma linha excluida** — comentada + nova linha
- [ ] Prefixo de demanda em alteracoes comentadas
- [ ] **Sem verbos validos comentados** (`-IF`, `-MOVE`, etc.)
- [ ] Cabecalho descritivo em exclusoes

## 04 — Condicionais / fluxo

- [ ] PERFORM entre SECTIONs; GO TO interno
- [ ] Null indicators tratados

## 05 — MOVE / conversao

- [ ] Sem MOVE formato/tamanho incompativeis
- [ ] PIC X → 9 via grupo
- [ ] Sinal via REDEFINES (RUCA24XC)

## 06 — Aritmetica / indices

- [ ] Contas com compactacao adequada
- [ ] Indice de OCCURS adequado

## 07 — Arquivos (se batch com arquivos)

- [ ] FILE STATUS + teste apos I/O
- [ ] EOF '10' sem erro
- [ ] POOL7100 em falha

## 08 — Copybook (se .cpy alterado)

- [ ] Hierarquia completa comentada em exclusoes
- [ ] Sem referencia ativa a campo excluido
- [ ] SICOR: ordem/tags conforme iniciativa 5.12

## 09 — SQL (se EXEC SQL alterado)

- [ ] DCLGEN confere com tabela no SQL

## 10 — Variaveis / README

- [ ] Sem variavel elementar apenas receptora (quality #8)
- [ ] README: plano validado, impacto confirmado, dependencias LIBS
- [ ] Pendencias LIBS justificadas
- [ ] Certificacao de riscos (Fase 4b): secao preenchida; nenhum **REALIZADO** em risco de codificacao

---

## Criterio de aprovacao

| Resultado | Condicao |
|---|---|
| **REPROVADO** | Qualquer BLOQUEANTE |
| **APROVADO COM RESSALVAS** | Apenas AVISO documentado |
| **APROVADO** | Todos OK ou N/A |

## Referencia no plano de ajuste

No README da demanda, citar capitulo por item:

```markdown
| # | Fonte | Alteracao | Referencia |
| 1 | I#RUECC5 | Excluir grupo | cap. 03 + cap. 08 |
| 2 | RUEC8831 | Novo MOVE | cap. 03 + cap. 01 + cap. 05 |
```
