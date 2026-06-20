# Guia de Organizacao e Seguranca

## Layout do repositorio Git

O repositorio `codebase-cobol` tem esta estrutura:

```text
codebase-cobol/          raiz Git e workspace Cursor
├── zapp.json            ZAPP canonico (IBM Z Open Editor)
├── .cursor/             skills e rules do agente
└── Cobol/               raiz logica do projeto COBOL
    ├── Projetos/
    ├── LIBS/
    └── zapp.json        fallback se abrir so a pasta Cobol/
```

Paths em skills, rules e scripts usam prefixo `Cobol/` a partir da raiz do repo.
Abrir a pasta `codebase-cobol` (ou o arquivo `codebase-cobol.code-workspace` na raiz).

## Estrutura recomendada

### Capgemini (alocacao)
- `Projetos/Capgemini/AGROLEG-nnn/<CENTRO_CUSTO>/COBOL`
- `Projetos/Capgemini/AGROLEG-nnn/<CENTRO_CUSTO>/JCL`
- (idem DB2, CICS, IMS)

### Stefanini / 7Comm (projeto) — pastas por componente

Na raiz da demanda, organizar por **tipo de componente** (nao por centro de custo):

```text
Projetos/<Consultoria>/<ID>/
├── README.md
├── intake.md
├── pgm/          programas COBOL (.cbl)
├── cpy/          copybooks / layouts
├── jcl/          jobs
├── net/          malha batch (exports de rede de jobs, ex. LIG1001.txt)
├── mfs/          mapas BMS (quando houver)
├── db2/          DCLGEN / SQL (quando houver)
└── ...
```

Centros de custo ficam no README (`## Centros de custo impactados`), nao no nome da pasta.

### Subpasta `analise/` (somente leitura / comparacao)

Quando baixar fontes **apenas para analise** (sem subida), usar:

```text
<componente>/analise/<member>
```

Exemplos:
- `cpy/analise/i#cbon01-prd.cpy` — layout PRD para comparar
- `pgm/analise/SLIG5000.cbl` — programa de contexto, fora do escopo de alteracao
- `jcl/analise/SLIG1067.jcl` — JCL de referencia

Regras:
- **Escopo (alteracao / entrega):** pasta do componente na raiz (`pgm/`, `cpy/`, …)
- **Somente analise:** `<componente>/analise/`
- **Nao** criar `analise/` na raiz da demanda
- Agente **nao altera** arquivos em `*/analise/` salvo pedido explicito
- `net/` recebe artefatos de malha (schedule, exports); insumo de analise, nao fonte COBOL de subida

## Regras de seguranca
- Nao armazenar senha/token/chave em arquivos do workspace.
- Substituir por marcador: `[consultar cofre]`.
- Manter credenciais apenas em cofre seguro.

## Padrao de anotacoes
- Arquivo tecnico: comandos, SQL, ids de programa, observacoes.
- Arquivo de credenciais: nao criar no workspace.
- README padrao: `Projetos/_indice/template-README-demanda.md`
- Referencia COBOL (dev + gate): `Projetos/_indice/referencia-cobol/README.md`
- Malha batch (JOB/JCL/PGM): `Projetos/_indice/referencia-batch-malha/README.md`
- Pipeline de demandas: skill `demanda-workflow` em `.cursor/skills/`
- Guia de fluxo AGROLEG: `Projetos/_indice/guia-fluxo-agroleg.md`
- Acervo de conhecimento: `.cursor/skills/agroleg-knowledge/`
