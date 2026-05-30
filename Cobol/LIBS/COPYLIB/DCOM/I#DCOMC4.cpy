      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C4 - DB2PRD.TOPER_AUTRZ_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C4-ARGUMENTOS-ENTRADA.

       05  C4-COLUNAS-DA-TABELA.
           10  C4-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C4-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
  LT       10  C4-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  C4-CTPO-AUTRZ-ESPCL             PIC S9(3)V COMP-3.
           10  C4-HULT-ATULZ                   PIC X(26).
           10  C4-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  C4-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  C4-PTX-JURO-OPER-DESC           PIC S9(3)V9(5) COMP-3.
           10  C4-CINDCD-FREQ-TX               PIC X(2).
           10  C4-CCTRO-CUSTO                  PIC X(4).
           10  C4-CAUTRZ-TX-TESOU              PIC S9(9)V COMP-3.
           10  C4-IAUTRZ-TX-TESOU              PIC X(40).
           10  C4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C4-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C4-RETORNO.
       05  C4-COD-RETORNO                         PIC 9(04).
       05  C4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C4-PROXIMO-RESTART                     PIC 9(05).
       05  C4-QTDE-RETORNADA                      PIC 9(03).
       05  C4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC4.
           10  C4RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C4RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
  LT       10  C4RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  C4RT-CTPO-AUTRZ-ESPCL           PIC S9(3)V COMP-3.
           10  C4RT-HULT-ATULZ                 PIC X(26).
           10  C4RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  C4RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  C4RT-PTX-JURO-OPER-DESC         PIC S9(3)V9(5) COMP-3.
           10  C4RT-CINDCD-FREQ-TX             PIC X(2).
           10  C4RT-CCTRO-CUSTO                PIC X(4).
           10  C4RT-CAUTRZ-TX-TESOU            PIC S9(9)V COMP-3.
           10  C4RT-IAUTRZ-TX-TESOU            PIC X(40).
           10  C4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C4RT-CTERM                      PIC X(8).
