      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H4 - DB2PRD.TINFO_TEMPR_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
      *          BRQ DIGITAL SOLUTIONS   -   ALTERACAO                *
      *---------------------------------------------------------------*
      *    ANALISTA:  LUCIANDRA SILVEIRA                              *
      *    DATA....:  26/12/2018                                      *
      *---------------------------------------------------------------*
      *    OBJETIVO:  ADEQUAR O CAMPO WBLOCO-INFO-TEMPR AO TAMANHO DA *
      *               TABELA DCOMB0H4.                                *
      *===============================================================*

       01  H4-ARGUMENTOS-ENTRADA.

       05  H4-COLUNAS-DA-TABELA.
           10  H4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H4-CTERM                        PIC X(8).
           10  H4-HARMAZ-INFO-TEMPR            PIC X(26).
           10  H4-NSEQ-INFO-TEMPR              PIC S9(5)V COMP-3.
FVB        10  H4-CTRANS-PROCM-SIST            PIC X(8).
BSI001     10  H4-WBLOCO-INFO-TEMPR            PIC X(1600).
           10  H4-WINFO-TEMPR-COMPL            PIC X(50).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H4-RETORNO.
       05  H4-COD-RETORNO                         PIC 9(04).
       05  H4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H4-PROXIMO-RESTART                     PIC 9(05).
       05  H4-QTDE-RETORNADA                      PIC 9(03).
       05  H4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH4.
           10  H4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H4RT-CTERM                      PIC X(8).
           10  H4RT-HARMAZ-INFO-TEMPR          PIC X(26).
           10  H4RT-NSEQ-INFO-TEMPR            PIC S9(5)V COMP-3.
FVB        10  H4RT-CTRANS-PROCM-SIST          PIC X(8).
BSI001     10  H4RT-WBLOCO-INFO-TEMPR          PIC X(1600).
           10  H4RT-WINFO-TEMPR-COMPL          PIC X(50).
