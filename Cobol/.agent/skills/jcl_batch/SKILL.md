---
name: jcl_batch
description: Skills for JCL batch development with COBOL, DD parameters, utilities, and best practices for mainframe batch jobs.
---

# JCL e Batch COBOL

Esta skill orienta a criação e a manutenção de **JCL** para jobs batch que executam programas COBOL, incluindo parâmetros DD, utilitários comuns e boas práticas.

## Declarações JCL principais

| Declaração | Função |
| :--- | :--- |
| **JOB** | Identifica o job e parâmetros de conta/prioridade. |
| **EXEC** | Define o passo (programa ou procedure) a executar. |
| **DD** | Define data sets (arquivos) usados no passo. |

## Parâmetros DD essenciais

### DSN (Data Set Name)

Nome do data set. Obrigatório para data sets existentes.

```jcl
//ARQUIVO  DD  DSN=PROJETO.ARQUIVO.DADOS,DISP=SHR
```

### DISP (Disposition)

Status do data set e ação ao final do passo (normal ou anormal).

| Valor | Significado |
| :--- | :--- |
| **SHR** | Compartilhado (leitura). |
| **OLD** | Exclusivo (leitura/escrita). |
| **NEW** | Criar novo. |
| **MOD** | Acrescentar a existente ou criar. |
| **(CATLG,DELETE)** | Manter no catálogo e deletar ao final. |
| **(PASS,DELETE)** | Passar ao próximo passo e deletar ao final. |

Exemplo para arquivo novo temporário:

```jcl
//SAIDA    DD  DSN=&&TEMP,DISP=(NEW,PASS),
//             SPACE=(TRK,(1,1)),DCB=(RECFM=FB,LRECL=80)
```

### SPACE

Alocação de espaço em DASD para data set novo.

- **TRK** ou **CYL**: unidade (trilha/cilindro).
- **(primária,(secundária,diretório))** para PDS; **(primária,secundária)** para sequencial.

Exemplo:

```jcl
//PDS      DD  DSN=MEU.PDS,DISP=(NEW,CATLG),
//             SPACE=(TRK,(5,2,10)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
```

### DCB (Data Control Block)

Define formato do data set (novo ou override).

| Subparâmetro | Uso |
| :--- | :--- |
| **RECFM** | FB, F, VB, VBS, etc. |
| **LRECL** | Tamanho lógico do registro (bytes). |
| **BLKSIZE** | Tamanho do bloco (opcional em muitos casos). |

> [!IMPORTANT] **Recomendação IBM para COBOL**
> - Para **arquivos novos**, **não** codificar BLKSIZE no JCL; usar no programa COBOL `BLOCK CONTAINS 0` ou opção **BLOCK0** do compilador para o z/OS definir o bloco.
> - Para data sets bloqueados já existentes, usar o BLKSIZE já definido (pode ser omitido no JCL se o programa/DCB do data set já o definem).

## Utilitários JCL mais usados

| Utilitário | Uso principal |
| :--- | :--- |
| **IEBGENER** | Cópia sequencial, geração de dados, conversão RECFM/LRECL. |
| **IEBCOPY** | Cópia de PDS ou membros (merge, backup). |
| **IDCAMS** | Catálogo, VSAM (define, delete, listcat, repro). |
| **SORT/DFSORT** | Ordenação, merge, cópia com seleção/cálculos. |

### Exemplo IEBGENER (cópia)

```jcl
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD  DSN=ENTRADA,DISP=SHR
//SYSUT2   DD  DSN=SAIDA,DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),DCB=(RECFM=FB,LRECL=80)
//SYSIN    DD  DUMMY
```

### Exemplo SORT

```jcl
//SORTSTEP EXEC PGM=SORT
//SORTIN   DD  DSN=ARQUIVO.ENTRADA,DISP=SHR
//SORTOUT  DD  DSN=ARQUIVO.ORDENADO,DISP=(NEW,CATLG),
//             SPACE=(TRK,(5,1)),DCB=(RECFM=FB,LRECL=80)
//SYSIN    DD  *
  SORT FIELDS=(1,10,CH,A)
  OPTION COPY
/*
```

## Técnicas avançadas úteis

| Técnica | Benefício |
| :--- | :--- |
| **GDG** (Generation Data Groups) | Versionar data sets (ex.: relatórios diários). |
| **Parâmetros simbólicos** | JCL reutilizável com alteração no JOB ou procedure. |
| **Procedures catalogadas** | Padronizar passos repetidos (compile, link, run). |
| **JCLLIB** | Incluir JCL de bibliotecas (procedures, includes). |
| **COND / IF-THEN-ELSE** | Execução condicional de passos. |
| **Restart** | Recuperação de jobs longos a partir de um passo. |

## Padrões e boas práticas

1. **Nomenclatura**: usar convenção de nomes para data sets e steps (ex.: STEP1, STEP2 ou COMPILE, LINK, RUN).
2. **Comentários**: usar `//*` para documentar propósito do step ou de DDs críticos.
3. **DISP**: garantir DISP adequado para não deixar data sets em estado inconsistente (evitar KEEP sem CATLG quando o uso for temporário).
4. **Performance**: alocar SPACE adequado; em SORT, fornecer SORTWK em volume separado quando necessário.
5. **Debug**: usar SYSOUT=* para listagens durante desenvolvimento; em produção, direcionar para data set ou classe de saída apropriada.
6. **Condição de passo**: usar COND apenas quando necessário e documentar o motivo (evitar jobs que “somem” passos sem explicação).

## Job COBOL típico (compile + link + run)

```jcl
//JOBCOB   JOB (ACCT),'COBOL BATCH',CLASS=A,MSGCLASS=X
//COMPILE  EXEC PGM=IGYCRCTL,REGION=4M
//STEPLIB  DD  DSN=SYS1.COBOL.LIB,DISP=SHR
//SYSIN    DD  DSN=MEU.PROJETO(PGM1),DISP=SHR
//SYSLIN   DD  DSN=&&LOADSET,DISP=(NEW,PASS),...
// ...
//LINK     EXEC PGM=IEWL,COND=(8,LT,COMPILE)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSN=MEU.LOADLIB(PGM1),DISP=SHR
// ...
//RUN      EXEC PGM=PGM1,COND=((0,NE,COMPILE),(0,NE,LINK))
//ENTRADA  DD  DSN=MEU.DADOS.ENTRADA,DISP=SHR
//SAIDA    DD  SYSOUT=*
// ...
```

Ajustar nomes de data sets, procedures e parâmetros conforme o padrão do seu ambiente (compilador, link-editor e convenções do projeto).
