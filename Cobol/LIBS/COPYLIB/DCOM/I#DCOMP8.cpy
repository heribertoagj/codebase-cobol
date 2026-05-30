      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0P8 - DB2PRD.THIST_PCELA_BLETO                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7745 - SELECT         - DCOM7746 - FETCH             *
      *    - DCOM7747 - INSERT         - DCOM7748 - UPDATE            *
      *    - DCOM7749 - DELETE                                        *
      *===============================================================*

       01  P8-ARGUMENTOS-ENTRADA.
           05 P8-COLUNAS-DA-TABELA.
              10 P8-DANO-OPER-DESC       PIC S9(004)V COMP-3.
              10 P8-NSEQ-OPER-DESC       PIC S9(009)V COMP-3.
              10 P8-NPCELA-DESC-COML     PIC S9(005)V COMP-3.
              10 P8-DINCL-BLETO-PGTO     PIC  X(010).
              10 P8-HULT-ATULZ           PIC  X(026).
              10 P8-DVCTO-BLETO-PGTO     PIC  X(010).
              10 P8-CNEGOC-COBR          PIC S9(018)V COMP-3.
              10 P8-CTITLO-COBR-BCO      PIC S9(018)V COMP-3.
              10 P8-VPRINC-BLETO-PGTO    PIC S9(015)V9(002) COMP-3.
              10 P8-VSDO-DVDOR-PCELA     PIC S9(015)V9(002) COMP-3.
              10 P8-HGERAC-BLETO-PGTO    PIC  X(026).
              10 P8-CFUNC-BDSCO          PIC S9(009)V COMP-3.
              10 P8-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 P8-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 P8-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  P8-RETORNO.
           05 P8-COD-RETORNO           PIC  9(004).
           05 P8-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 P8-PROXIMO-RESTART       PIC  9(005).
           05 P8-QTDE-RETORNADA        PIC  9(003).
           05 P8-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 P8-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TP8.
              10 P8RT-DANO-OPER-DESC       PIC S9(004)V COMP-3.
              10 P8RT-NSEQ-OPER-DESC       PIC S9(009)V COMP-3.
              10 P8RT-NPCELA-DESC-COML     PIC S9(005)V COMP-3.
              10 P8RT-DINCL-BLETO-PGTO     PIC  X(010).
              10 P8RT-HULT-ATULZ           PIC  X(026).
              10 P8RT-DVCTO-BLETO-PGTO     PIC  X(010).
              10 P8RT-CNEGOC-COBR          PIC S9(018)V COMP-3.
              10 P8RT-CTITLO-COBR-BCO      PIC S9(018)V COMP-3.
              10 P8RT-VPRINC-BLETO-PGTO    PIC S9(015)V9(002) COMP-3.
              10 P8RT-VSDO-DVDOR-PCELA     PIC S9(015)V9(002) COMP-3.
              10 P8RT-HGERAC-BLETO-PGTO    PIC  X(026).
              10 P8RT-CFUNC-BDSCO          PIC S9(009)V COMP-3.
              10 P8RT-CTERM                PIC  X(008).
