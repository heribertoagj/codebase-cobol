---
name: demanda-pre-subida
description: Gate unificado de pre-subida para demandas COBOL AGROLEG — consolida quality code Hexavision, SICOR 5.12, validacao DCLGEN/SQL, dependencias LIBS e checklist do README. Use antes de promover, subir, entregar ou marcar demanda como pronta para subida.
---

# Demanda Pre-Subida

## Objetivo

Executar um gate unico antes de promover fontes, consolidando todas as validacoes relevantes em um relatorio padronizado.

**Padrao cliente (nao simplificar):** este gate permanece unificado — ver [../README.md](../README.md#nao-simplificar--padroes-do-cliente-obrigatorios).

## Quando usar

- Usuario pede "pre-subida", "pronto para subida", "validar antes de promover".
- Fase 5 do `demanda-workflow` (`Pre-subida AGROLEG-nnn`).
- Apos certificacao de riscos (Fase 4b), antes de subida MF.

## Entradas

- Caminho da demanda (ex.: `Cobol/Projetos/Capgemini/AGROLEG-1239`, `Cobol/Projetos/Stefanini/STEF-001`, `Cobol/Projetos/7Comm/7COMM-001`).
- Fontes alterados (`.cbl`, `.cpy`, `.mfs`, `.jcl`).
- README da demanda.

## Fluxo de validacao

Execute nesta ordem. Checklist detalhado: `Cobol/Projetos/_indice/referencia-cobol/10-checklist-gate.md`

### 0. Certificacao de riscos (Fase 4b)

- [ ] Secao `## Certificacao de riscos (pos-desenvolvimento)` preenchida no README
- [ ] Nenhum veredito **REALIZADO** em risco de codificacao
- [ ] Se ausente ou reprovada: orientar `Certificar riscos <ID>` — nao marcar pronto para subida

### 1. README e escopo

- [ ] `## Objetivo` preenchido
- [ ] `## Plano de ajuste (proposta)` preenchido e validado pelo usuario (Fase 3)
- [ ] `## Impacto tecnico (confirmado)` preenchido
- [ ] `## Dependencias LIBS` atualizado (rodar script com `-InPlace` se necessario)
- [ ] Pendencias em LIBS justificadas ou resolvidas

### 2. Quality code (Hexavision)

Aplicar `Cobol/Projetos/_indice/referencia-cobol/10-checklist-gate.md` (caps. 03–08) e skill `bradesco-quality-code`.

### 3. SICOR 5.12 (se aplicavel)

Se README indicar iniciativa SICOR 5.12 ou mensagens COR/CIR, aplicar checklist em `agroleg-knowledge/initiatives/SICOR-5.12.md`:

- campos excluidos removidos da serializacao
- grupos novos implementados por completo
- ordem de tags 5.12
- tipologias e cardinalidade
- exclusao hierarquica em copybook (`bradesco-quality-code` regra #10)

### 4. DCLGEN / SQL

Para cada `EXEC SQL` alterado:

- localizar DCLGEN correspondente em `Cobol/LIBS`
- confirmar nome real da tabela no `DECLARE ... TABLE`
- flagar divergencia entre SQL e DCLGEN

### 5. Estilo Bradesco

Verificar `Cobol/Projetos/_indice/referencia-cobol/01-estrutura-programa.md` e `bradesco-style`:

- SECTION com `-99-FIM. EXIT.`
- Indentacao col 20/40 em trechos novos
- Rastreio cap. 03 em alteracoes

### 6. CICS (se aplicavel)

Se houver `.mfs` ou `EXEC CICS`:

- RESP() em comandos criticos
- MAPFAIL tratado quando esperado
- copybook BMS incluido corretamente

## Formato de relatorio (obrigatorio)

```text
=== PRE-SUBIDA: AGROLEG-nnn ===

RESUMO: [APROVADO | APROVADO COM RESSALVAS | REPROVADO]

CERTIFICACAO DE RISCOS (Fase 4b)
- [OK|BLOQUEANTE|AVISO] ...

README
- [OK|BLOQUEANTE|AVISO] ...

QUALITY CODE (Hexavision)
- [OK|BLOQUEANTE|AVISO] ...

SICOR 5.12
- [OK|N/A|BLOQUEANTE|AVISO] ...

DCLGEN / SQL
- [OK|N/A|BLOQUEANTE|AVISO] ...

ESTILO
- [OK|BLOQUEANTE|AVISO] ...

DEPENDENCIAS LIBS
- [OK|BLOQUEANTE|AVISO] ...

ACOES NECESSARIAS
1. ...
```

## Criterio de aprovacao

- **REPROVADO**: qualquer item BLOQUEANTE em quality, SICOR ou DCLGEN.
- **APROVADO COM RESSALVAS**: apenas AVISO (ex.: pendencia LIBS justificada).
- **APROVADO**: todos OK ou N/A.

## Pos-acao

Se aprovado, atualizar README:

```markdown
## Status
- [x] Em andamento
- [x] Em teste
- [x] Pronto para subida
```

Proximo passo: **Fase 5b — Subida MF** (`demanda-workflow`). Solicitar ao usuario pacote de **subida**, pacote de **contingencia**, members, status do compile e **ticket Jira de promocao**; preencher `## Pacotes changeman` e `## Promocao ambientes (Jira)` no README.

Se reprovado, listar correcoes objetivas e nao marcar pronto para subida.
