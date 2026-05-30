      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K5 - DB2PRD.TESTOQ_OPER_ATIVO      *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K5-ARGUMENTOS-ENTRADA.

       05  K5-COLUNAS-DA-TABELA.

           10  K5-CCNPJ-CPF              PIC S9(09)V COMP-3.
           10  K5-CFLIAL-CNPJ            PIC S9(05)V COMP-3.
           10  K5-CPRODT                 PIC S9(03)V COMP-3.
           10  K5-CSPROD-DESC-COML       PIC S9(03)V COMP-3.
           10  K5-CCONTR-CONVE-DESC      PIC S9(09)V COMP-3.
           10  K5-CELMTO-DESC-COML       PIC S9(03)V COMP-3.
           10  K5-CCTRL-CNPJ-CPF         PIC S9(02)V COMP-3.
           10  K5-QOPER-ATIVO-ESTOQ      PIC S9(09)V COMP-3.
           10  K5-VOPER-ATIVO-ESTOQ      PIC S9(13)V9(2) COMP-3.
           10  K5-DINCL-REG              PIC  X(10).
           10  K5-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  K5-CTERM                  PIC  X(08).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K5-QTDE-A-DESPREZAR           PIC  9(05).

      * PREENCHER O CAMPO ABAIXO
      *-----------------------------------------------*
       05  K5-INSTRUCAO                  PIC  X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K5-RETORNO.
       05  K5-COD-RETORNO                PIC  9(04).
       05  K5-MENSAGEM                   PIC  X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K5-PROXIMO-RESTART            PIC  9(05).
       05  K5-QTDE-RETORNADA             PIC  9(03).
       05  K5-QTDE-TOTAL                 PIC  9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK5.
           10  K5RT-CCNPJ-CPF            PIC S9(09)V COMP-3.
           10  K5RT-CFLIAL-CNPJ          PIC S9(05)V COMP-3.
           10  K5RT-CPRODT               PIC S9(03)V COMP-3.
           10  K5RT-CSPROD-DESC-COML     PIC S9(03)V COMP-3.
           10  K5RT-CCONTR-CONVE-DESC    PIC S9(09)V COMP-3.
           10  K5RT-CELMTO-DESC-COML     PIC S9(03)V COMP-3.
           10  K5RT-CCTRL-CNPJ-CPF       PIC S9(02)V COMP-3.
           10  K5RT-QOPER-ATIVO-ESTOQ    PIC S9(09)V COMP-3.
           10  K5RT-VOPER-ATIVO-ESTOQ    PIC S9(13)V9(2) COMP-3.
           10  K5RT-DINCL-REG            PIC  X(10).
           10  K5RT-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  K5RT-CTERM                PIC  X(08).

