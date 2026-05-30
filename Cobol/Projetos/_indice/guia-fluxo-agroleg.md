# Guia de Fluxo de Demandas (usuario + agente)

Referencia rapida do ciclo ponta a ponta. Detalhes nas skills `demanda-workflow` e `demanda-intake-reuniao`.

## Canais de entrada

| Consultoria | Como chega | Comando inicial |
|---|---|---|
| Capgemini | XML Jira | `Implementar AGROLEG-nnn.xml` |
| Stefanini | Reuniao | `Iniciar demanda Stefanini — [notas]` |
| 7Comm | Reuniao | `Iniciar demanda 7Comm — [notas]` |

## Visao geral

```
Capgemini:  XML Jira → Inicializacao → ...
Stefanini:  Reuniao → Intake (Fase 0) → Inicializacao → ...
7Comm:      Reuniao → Intake (Fase 0) → Inicializacao → ...

[comum] → [loop] Baixa fontes + Analise → Plano → Implementacao → Pre-subida → Homologacao → Finalizar
```

## Seus passos

| # | Voce faz | Comando para o agente |
|---|---|---|
| 0 | (Stefanini/7Comm) Descreve reuniao | `Iniciar demanda Stefanini` ou `Iniciar demanda 7Comm` |
| 1 | (Capgemini) Baixa XML Jira | `Implementar AGROLEG-nnn.xml` |
| 2 | Baixa fontes P1 que o agente listou | `Fontes baixados — continuar analise <ID>` |
| 3 | Repete passo 2 ate analise concluida | (mesmo comando) |
| 4 | Revisa plano no README | `Gerar plano de ajuste <ID>` (se ainda nao gerado) |
| 5 | Valida plano | `Implementar plano <ID>` |
| 6 | Valida implementacao | `Pre-subida <ID>` |
| 7 | Sobe fontes para MF e homologa | (humano — informar status) |
| 8 | Apos implantacao | `Finalizar <ID>` |

## Gates (o agente para e espera voce)

1. **Apos inicializacao** — baixar fontes antes de analise profunda
2. **Apos analise parcial** — baixar fontes faltantes
3. **Apos plano** — validar antes de implementar
4. **Apos pre-subida** — voce sobe MF
5. **Apos homologacao** — confirmar implantacao antes de encerrar

## O agente NAO faz

- Implementar codigo ao receber apenas o XML
- Implementar sem voce dizer `Implementar plano <ID>`
- Encerrar acervo antes da homologacao implantada
- Subir fontes no mainframe

## Estrutura da pasta

```
Projetos/Capgemini/AGROLEG-nnn/
├── AGROLEG-nnn.xml
├── README.md
└── pgm/ cpy/ jcl/ mfs/

Projetos/Stefanini/STEF-nnn/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/  ...
└── <componente>/analise/   (fontes so para analise)

Projetos/7Comm/7COMM-nnn/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/  ...
└── cpy/analise/  pgm/analise/  ...   (conforme necessidade)
```

Pastas por **componente** (nao por centro de custo). Detalhes: `guia-organizacao-seguranca.md`.

## Referencia COBOL (desenvolvimento e gate)

Manual completo: `Projetos/_indice/referencia-cobol/README.md`

Fase 4 (implementacao) e Fase 5 (pre-subida) devem seguir caps. 01–10.

Malha batch (escopo BATCH): `Projetos/_indice/referencia-batch-malha/README.md`

## Secoes importantes do README

| Secao | Quando preenche |
|---|---|
| Status workflow | Sempre atualizado pelo agente |
| Fontes pendentes baixa | Fase 1 e 2 |
| Malha batch | Fase 2 (BATCH) |
| Plano de ajuste (proposta) | Fase 3 |
| Impacto tecnico (confirmado) | Fase 2 (analise) + Fase 4 (implementacao) |
| Licoes para proximas analises | Fase 7 |
