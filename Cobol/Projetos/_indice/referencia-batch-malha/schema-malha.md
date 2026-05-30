# Schema — Malha Batch (JSON)

## Objeto raiz

| Campo | Tipo | Obrigatorio | Descricao |
|---|---|---|---|
| `malha` | objeto | sim | Metadados da malha |
| `jobs` | array | sim | Lista de JOBs |
| `arquivos_compartilhados` | array | nao | Arquivos que conectam mais de um JOB |

## malha

| Campo | Tipo | Descricao |
|---|---|---|
| `id` | string | Identificador unico (ex.: `liberacao-cor0005-rura`) |
| `demanda` | string | Chave AGROLEG (ex.: `AGROLEG-1239`) |
| `titulo` | string | Titulo do fluxo |
| `descricao` | string | Descricao funcional |
| `atualizado_em` | string | Data ISO ou texto livre |

## jobs[]

| Campo | Tipo | Descricao |
|---|---|---|
| `id` | string | ID interno (ex.: `JOB-A`) — usado no diagrama |
| `jcl` | string | Member JCL |
| `nome_job` | string | Nome do JOB no JCL |
| `resumo` | string | O que o JOB faz (1-3 linhas) |
| `dispara` | array[string] | IDs de JOBs acionados apos este (seta no diagrama) |
| `chamado_por` | string | ID do JOB pai (opcional, redundante com `dispara`) |
| `steps` | array | Steps / programas executados |

## jobs[].steps[]

| Campo | Tipo | Descricao |
|---|---|---|
| `step` | string | Nome do step (ex.: `S010`) |
| `programa` | string | PGM= |
| `resumo` | string | Funcao do programa no step |
| `arquivos` | array | DDs do step |

## jobs[].steps[].arquivos[]

| Campo | Tipo | Descricao |
|---|---|---|
| `ddname` | string | DDNAME |
| `operacao` | string | `READ`, `WRITE`, `UPDATE`, `INOUT` |
| `dataset` | string | DSN ou descricao (GDG, VSAM, temp) |
| `lrecl` | number | LRECL |
| `blocos` | number | BLKSIZE (opcional) |
| `observacao` | string | Origem/destino, layout, etc. |

## arquivos_compartilhados[]

| Campo | Tipo | Descricao |
|---|---|---|
| `id` | string | ID do arquivo no diagrama |
| `ddname` | string | DDNAME principal |
| `lrecl` | number | LRECL |
| `usado_por` | array[string] | IDs dos JOBs |
| `observacao` | string | Papel do arquivo na malha |

## Exemplo minimo

Ver `template-malha.json` e `malhas/AGROLEG-1239-liberacao.json`.
