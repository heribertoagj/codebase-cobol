      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I6-ARGUMENTOS-ENTRADA.

       05  I6-COLUNAS-DA-TABELA.
           10  I6-CPRODT                   PIC S9(03)V COMP-3.
           10  I6-CSPROD-DESC-COML         PIC S9(03)V COMP-3.
           10  I6-DINCL-REG                PIC  X(10).
           10  I6-CTPO-PSSOA               PIC  X(01).
           10  I6-CSIT-DESC-COML           PIC S9(03)V COMP-3.
           10  I6-DINIC-VGCIA-CUSTO        PIC  X(10).
           10  I6-DFIM-VGCIA-CUSTO         PIC  X(10).
           10  I6-CIDTFD-CUSTO-INTRN       PIC S9(05)V COMP-3.
           10  I6-HULT-ATULZ               PIC  X(26).
           10  I6-CFUNC-BDSCO              PIC S9(09)V COMP-3.
           10  I6-CTERM                    PIC  X(08).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I6-QTDE-A-DESPREZAR             PIC  9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I6-INSTRUCAO                    PIC  X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I6-RETORNO.
       05  I6-COD-RETORNO                  PIC  9(04).
       05  I6-MENSAGEM                     PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I6-PROXIMO-RESTART              PIC  9(05).
       05  I6-QTDE-RETORNADA               PIC  9(03).
       05  I6-QTDE-TOTAL                   PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI6.
           10  I6RT-CPRODT                 PIC S9(03)V COMP-3.
           10  I6RT-CSPROD-DESC-COML       PIC S9(03)V COMP-3.
           10  I6RT-DINCL-REG              PIC  X(10).
           10  I6RT-CTPO-PSSOA             PIC  X(01).
           10  I6RT-CSIT-DESC-COML         PIC S9(03)V COMP-3.
           10  I6RT-DINIC-VGCIA-CUSTO      PIC  X(10).
           10  I6RT-DFIM-VGCIA-CUSTO       PIC  X(10).
           10  I6RT-CIDTFD-CUSTO-INTRN     PIC S9(05)V COMP-3.
           10  I6RT-HULT-ATULZ             PIC  X(26).
           10  I6RT-CFUNC-BDSCO            PIC S9(09)V COMP-3.
           10  I6RT-CTERM                  PIC  X(08).
