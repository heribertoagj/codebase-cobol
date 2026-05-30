      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K1 - DB2PRD.TLOTE_RPROC_PGTO       *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K1-ARGUMENTOS-ENTRADA.

       05  K1-COLUNAS-DA-TABELA.
           10  K1-NSEQ-REG-MOVTO       PIC S9(012)       COMP-3.
           10  K1-DPROCM-MOVTO-BASIL   PIC  X(010).
           10  K1-CRPROC-MOVTO-BASIL   PIC  X(001).
           10  K1-CTPO-LYOUT-CABEC     PIC  X(001).
           10  K1-CEMPR-INTFC-BASIL    PIC S9(001)       COMP-3.
           10  K1-CCTRO-CUSTO          PIC  X(004).
           10  K1-NSEQ-REG-CABEC       PIC S9(012)       COMP-3.
           10  K1-CERRO-REG-CABEC      PIC  X(032).
           10  K1-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K1-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K1-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K1-CTPO-LYOUT-RODAP     PIC  X(001).
           10  K1-QREG-DETLH-MOVTO     PIC S9(009)       COMP-3.
           10  K1-QREG-TOT-MOVTO       PIC S9(009)       COMP-3.
           10  K1-NSEQ-REG-RODAP       PIC S9(012)       COMP-3.
           10  K1-CERRO-REG-RODAP      PIC  X(032).
           10  K1-HATULZ               PIC  X(026).
           10  K1-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K1-CTERM                PIC  X(008).
           10  K1-DINCL-REG            PIC  X(010).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K1-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K1-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K1-RETORNO.
       05  K1-COD-RETORNO              PIC 9(04).
       05  K1-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K1-PROXIMO-RESTART          PIC 9(05).
       05  K1-QTDE-RETORNADA           PIC 9(03).
       05  K1-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K1RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK1.
           10  K1RT-NSEQ-REG-MOVTO       PIC S9(012)       COMP-3.
           10  K1RT-DPROCM-MOVTO-BASIL   PIC  X(010).
           10  K1RT-CRPROC-MOVTO-BASIL   PIC  X(001).
           10  K1RT-CTPO-LYOUT-CABEC     PIC  X(001).
           10  K1RT-CEMPR-INTFC-BASIL    PIC S9(001)       COMP-3.
           10  K1RT-CCTRO-CUSTO          PIC  X(004).
           10  K1RT-NSEQ-REG-CABEC       PIC S9(012)       COMP-3.
           10  K1RT-CERRO-REG-CABEC      PIC  X(032).
           10  K1RT-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K1RT-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K1RT-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K1RT-CTPO-LYOUT-RODAP     PIC  X(001).
           10  K1RT-QREG-DETLH-MOVTO     PIC S9(009)       COMP-3.
           10  K1RT-QREG-TOT-MOVTO       PIC S9(009)       COMP-3.
           10  K1RT-NSEQ-REG-RODAP       PIC S9(012)       COMP-3.
           10  K1RT-CERRO-REG-RODAP      PIC  X(032).
           10  K1RT-HATULZ               PIC  X(026).
           10  K1RT-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K1RT-CTERM                PIC  X(008).
           10  K1RT-DINCL-REG            PIC  X(010).

