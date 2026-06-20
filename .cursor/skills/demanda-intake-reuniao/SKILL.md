---
name: demanda-intake-reuniao
description: Inicia demandas Stefanini (STEF-{NNN}) e 7Comm (7COMM-{NNN}) a partir de reuniao ou descricao livre — gera ID sequencial, intake.md e README inicial. Use quando o usuario disser Iniciar demanda Stefanini, Iniciar demanda 7Comm, ou colar notas de reuniao para STEF/7COMM.
---

# Demanda Intake (Reuniao — Stefanini / 7Comm)

## Objetivo

Substituir o XML Jira da Capgemini por um **questionario de intake** para demandas que chegam via reuniao.

## Convencao de ID

| Consultoria | Pasta | Padrao ID | Exemplo |
|---|---|---|---|
| Stefanini | `Cobol/Projetos/Stefanini/` | `STEF-{NNN}` | STEF-001 |
| 7Comm | `Cobol/Projetos/7Comm/` | `7COMM-{NNN}` | 7COMM-001 |

- **NNN** = sequencial de 3 digitos por consultoria (001, 002…).
- **Centro de custo NAO entra no ID** — listar todos em `## Centros de custo impactados` no README.
- Titulo legivel no cabecalho do README: `# STEF-026 - [titulo]`.

## Comando do usuario

```text
Iniciar demanda Stefanini — [notas da reuniao ou respostas ao questionario]
Iniciar demanda 7Comm — [notas da reuniao ou respostas ao questionario]
```

Comandos das Fases 2–7 usam o ID gerado (mesmo padrao AGROLEG):

```text
Fontes baixados — continuar analise STEF-001
Gerar plano de ajuste 7COMM-001
Implementar plano STEF-001
Pre-subida 7COMM-001
Finalizar STEF-001
```

## Fase 0 — Intake (este skill)

**Disparo:** `Iniciar demanda Stefanini` ou `Iniciar demanda 7Comm`

### Passos

1. Identificar consultoria (Stefanini → STEF, 7Comm → 7COMM).
2. Se notas da reuniao forem vagas ou incompletas: acionar skill `brainstorming` (minimo 3 perguntas) **antes** de criar pasta.
3. Escanear `Cobol/Projetos/<Consultoria>/` e sugerir proximo ID livre.
4. Se usuario nao informou titulo, inferir das notas ou pedir confirmacao.
5. Criar pasta `Cobol/Projetos/<Consultoria>/<ID>/`.
6. Copiar `Cobol/Projetos/_indice/template-intake-reuniao.md` → `intake.md` e preencher com notas do usuario.
7. Copiar `Cobol/Projetos/_indice/template-README-demanda.md` → `README.md` e preencher secoes iniciais.
8. Preencher `## Dados de origem` (nao usar Dados Jira).
9. Montar `## Fontes pendentes baixa` preliminar.
10. Listar **perguntas em aberto**; se persistirem lacunas criticas, sugerir `Clarificar requisitos <ID>` (skill `clarification`).
11. Atualizar `Cobol/Projetos/_indice/catalogo-demandas.md`.
12. Atualizar `## Status workflow`: `Intake concluido — aguardando fontes ou analise`.

### Gate

Usuario confirma ID, titulo e escopo preliminar antes de Fase 2.

### Saida obrigatoria

```text
=== {ID} — INTAKE ===

ID SUGERIDO: STEF-00N / 7COMM-00N
PASTA: Cobol/Projetos/<Consultoria>/<ID>/
CENTROS DE CUSTO: (lista)
ESCOPO PRELIMINAR: ...
FONTES P1: ...
PERGUNTAS EM ABERTO: ...
PROXIMO: Fontes baixados — continuar analise {ID}
```

## Questionario

Template completo: `Cobol/Projetos/_indice/template-intake-reuniao.md`

Blocos: Identificacao, Negocio, Escopo tecnico, Contexto, Fontes, Regras, Testes, Notas.

## Estrutura da pasta (componentes)

Pastas por **tipo de componente** na raiz da demanda. Centro de custo fica no README, nao na pasta.

```text
Cobol/Projetos/Stefanini/STEF-nnn/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/  mfs/  db2/
└── <componente>/analise/    fontes baixadas so para analise

Cobol/Projetos/7Comm/7COMM-nnn/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/
└── cpy/analise/  pgm/analise/  ...
```

| Pasta | Conteudo |
|---|---|
| `pgm/` | Programas do escopo (alteracao) |
| `cpy/` | Copybooks do escopo |
| `jcl/` | Jobs do escopo |
| `net/` | Malha batch (exports rede jobs, schedule) |
| `<comp>/analise/` | Referencia/comparacao — **nao alterar** salvo pedido |

Detalhes: `Cobol/Projetos/_indice/guia-organizacao-seguranca.md`

## Integracao com demanda-workflow

Apos Fase 0, seguir skill `demanda-workflow` a partir da **Fase 1** (impacto preliminar) ou **Fase 2** (se fontes ja existirem), substituindo `AGROLEG-nnn` pelo ID STEF/7COMM.

Nao usar `jira-xml-readme-sync` neste canal.

## Proximo ID (referencia)

Consultar pastas existentes em `Cobol/Projetos/Stefanini/` e `Cobol/Projetos/7Comm/` antes de alocar.

Demandas atuais (migracao 2026-05-26):
- STEF-001 — GFCT CNPJ/FILIAL
- STEF-002 — CEPT2A16
- 7COMM-001 — SLIG CBON01 / cadeia SLIG1067
- 7COMM-002 — RUCA6630 / CNPJ RUCAT631
