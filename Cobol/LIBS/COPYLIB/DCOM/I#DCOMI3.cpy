      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I3 - DB2PRD.TTITLO_ANLSE_CREDT     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I3-ARGUMENTOS-ENTRADA.

       05  I3-COLUNAS-DA-TABELA.
           10  I3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  I3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  I3-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  I3-CPROT-REMSS-TITLO            PIC S9(12)V COMP-3.
           10  I3-DRECEB-REMSS-TITLO           PIC X(10).
           10  I3-NSEQ-PROT-TITLO              PIC S9(10)V COMP-3.
           10  I3-HANLSE-TITLO                 PIC X(26).
           10  I3-CTPO-ANLSE-TITLO             PIC S9(3)V COMP-3.
           10  I3-CTPO-IMPED-TITLO             PIC S9(3)V COMP-3.
           10  I3-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  I3-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  I3-CRESUL-ANLSE-TITLO           PIC S9(2)V COMP-3.
           10  I3-RREST-ANLSE-TITLO            PIC X(250).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I3-RETORNO.
       05  I3-COD-RETORNO                         PIC 9(04).
       05  I3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I3-PROXIMO-RESTART                     PIC 9(05).
       05  I3-QTDE-RETORNADA                      PIC 9(03).
       05  I3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI3.
           10  I3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  I3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  I3RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  I3RT-CPROT-REMSS-TITLO          PIC S9(12)V COMP-3.
           10  I3RT-DRECEB-REMSS-TITLO         PIC X(10).
           10  I3RT-NSEQ-PROT-TITLO            PIC S9(10)V COMP-3.
           10  I3RT-HANLSE-TITLO               PIC X(26).
           10  I3RT-CTPO-ANLSE-TITLO           PIC S9(3)V COMP-3.
           10  I3RT-CTPO-IMPED-TITLO           PIC S9(3)V COMP-3.
           10  I3RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  I3RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  I3RT-CRESUL-ANLSE-TITLO         PIC S9(2)V COMP-3.
           10  I3RT-RREST-ANLSE-TITLO          PIC X(250).
