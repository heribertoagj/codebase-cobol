      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0P9 - DB2PRD.TPCELA_BLETO_PGTO                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7750 - SELECT         - DCOM7751 - FETCH             *
      *    - DCOM7752 - INSERT         - DCOM7753 - UPDATE            *
      *    - DCOM7754 - DELETE                                        *
      *===============================================================*

       01  P9-ARGUMENTOS-ENTRADA.
           05 P9-COLUNAS-DA-TABELA.
              10 P9-DANO-OPER-DESC     PIC S9(004)V COMP-3.
              10 P9-NSEQ-OPER-DESC     PIC S9(009)V COMP-3.
              10 P9-NPCELA-DESC-COML   PIC S9(005)V COMP-3.
              10 P9-DINCL-BLETO-PGTO   PIC  X(010).
              10 P9-DVCTO-BLETO-PGTO   PIC  X(010).
              10 P9-CNEGOC-COBR        PIC S9(018)V COMP-3.
              10 P9-CTITLO-COBR-BCO    PIC S9(018)V COMP-3.
              10 P9-VPRINC-BLETO-PGTO  PIC S9(015)V9(002) COMP-3.
              10 P9-VSDO-DVDOR-PCELA   PIC S9(015)V9(002) COMP-3.
              10 P9-HGERAC-BLETO-PGTO  PIC  X(026).
              10 P9-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 P9-CTERM              PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 P9-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 P9-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  P9-RETORNO.
           05 P9-COD-RETORNO           PIC  9(004).
           05 P9-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 P9-PROXIMO-RESTART       PIC  9(005).
           05 P9-QTDE-RETORNADA        PIC  9(003).
           05 P9-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 P9-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TP9.
              10 P9RT-DANO-OPER-DESC    PIC S9(004)V COMP-3.
              10 P9RT-NSEQ-OPER-DESC    PIC S9(009)V COMP-3.
              10 P9RT-NPCELA-DESC-COML  PIC S9(005)V COMP-3.
              10 P9RT-DINCL-BLETO-PGTO  PIC  X(010).
              10 P9RT-DVCTO-BLETO-PGTO  PIC  X(010).
              10 P9RT-CNEGOC-COBR       PIC S9(018)V COMP-3.
              10 P9RT-CTITLO-COBR-BCO   PIC S9(018)V COMP-3.
              10 P9RT-VPRINC-BLETO-PGTO PIC S9(015)V9(002) COMP-3.
              10 P9RT-VSDO-DVDOR-PCELA  PIC S9(015)V9(002) COMP-3.
              10 P9RT-HGERAC-BLETO-PGTO PIC  X(026).
              10 P9RT-CFUNC-BDSCO       PIC S9(009)V COMP-3.
              10 P9RT-CTERM             PIC  X(008).
