      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K2 - DB2PRD.TPGTO_INCOT_BASIL      *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K2-ARGUMENTOS-ENTRADA.

       05  K2-COLUNAS-DA-TABELA.
           10  K2-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  K2-DPROCM-PGTO-INCOT    PIC  X(010).
           10  K2-CBCO                 PIC S9(003)       COMP-3.
           10  K2-CAG-BCRIA            PIC S9(005)       COMP-3.
           10  K2-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
           10  K2-CCART                PIC  X(005).
           10  K2-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
           10  K2-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
           10  K2-DVCTO-PCELA-DESC     PIC  X(010).
           10  K2-DEFETV-BAIXA-PCELA   PIC  X(010).
           10  K2-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
           10  K2-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
           10  K2-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
           10  K2-DANO-MES-BASE        PIC S9(006)       COMP-3.
           10  K2-CPROD                PIC S9(003)       COMP-3.
           10  K2-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
           10  K2-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K2-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K2-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K2-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
           10  K2-HATULZ               PIC  X(026).
           10  K2-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K2-CTERM                PIC  X(008).
           10  K2-DINCL-REG            PIC  X(010).
           10  K2-CSIT-PGTO-INCOT-W    PIC S9(001)       COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K2-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K2-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K2-RETORNO.
       05  K2-COD-RETORNO              PIC 9(04).
       05  K2-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K2-PROXIMO-RESTART          PIC 9(05).
       05  K2-QTDE-RETORNADA           PIC 9(03).
       05  K2-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K2RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK2.
           10  K2RT-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  K2RT-DPROCM-PGTO-INCOT    PIC  X(010).
           10  K2RT-CBCO                 PIC S9(003)       COMP-3.
           10  K2RT-CAG-BCRIA            PIC S9(005)       COMP-3.
           10  K2RT-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
           10  K2RT-CCART                PIC  X(005).
           10  K2RT-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
           10  K2RT-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
           10  K2RT-DVCTO-PCELA-DESC     PIC  X(010).
           10  K2RT-DEFETV-BAIXA-PCELA   PIC  X(010).
           10  K2RT-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
           10  K2RT-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
           10  K2RT-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
           10  K2RT-DANO-MES-BASE        PIC S9(006)       COMP-3.
           10  K2RT-CPROD                PIC S9(003)       COMP-3.
           10  K2RT-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
           10  K2RT-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K2RT-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K2RT-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K2RT-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
           10  K2RT-HATULZ               PIC  X(026).
           10  K2RT-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K2RT-CTERM                PIC  X(008).
           10  K2RT-DINCL-REG            PIC  X(010).



