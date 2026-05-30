      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K4 - DB2PRD.TPGTO_CORRC_BASIL      *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K4-ARGUMENTOS-ENTRADA.

       05  K4-COLUNAS-DA-TABELA.
           10  K4-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  K4-DPROCM-PGTO-INCOT    PIC  X(010).
           10  K4-CBCO                 PIC S9(003)       COMP-3.
           10  K4-CAG-BCRIA            PIC S9(005)       COMP-3.
           10  K4-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
           10  K4-CCART                PIC  X(005).
           10  K4-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
           10  K4-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
           10  K4-DVCTO-PCELA-DESC     PIC  X(010).
           10  K4-DEFETV-BAIXA-PCELA   PIC  X(010).
           10  K4-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
           10  K4-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
           10  K4-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
           10  K4-DANO-MES-BASE        PIC S9(006)       COMP-3.
           10  K4-CPROD                PIC S9(003)       COMP-3.
           10  K4-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
           10  K4-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K4-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K4-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K4-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
           10  K4-HATULZ               PIC  X(026).
           10  K4-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K4-CTERM                PIC  X(008).
           10  K4-DINCL-REG            PIC  X(010).
           10  K4-CSIT-PGTO-INCOT-W    PIC S9(001)       COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K4-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K4-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K4-RETORNO.
       05  K4-COD-RETORNO              PIC 9(04).
       05  K4-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K4-PROXIMO-RESTART          PIC 9(05).
       05  K4-QTDE-RETORNADA           PIC 9(03).
       05  K4-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K4RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK4.
           10  K4RT-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  K4RT-DPROCM-PGTO-INCOT    PIC  X(010).
           10  K4RT-CBCO                 PIC S9(003)       COMP-3.
           10  K4RT-CAG-BCRIA            PIC S9(005)       COMP-3.
           10  K4RT-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
           10  K4RT-CCART                PIC  X(005).
           10  K4RT-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
           10  K4RT-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
           10  K4RT-DVCTO-PCELA-DESC     PIC  X(010).
           10  K4RT-DEFETV-BAIXA-PCELA   PIC  X(010).
           10  K4RT-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
           10  K4RT-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
           10  K4RT-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
           10  K4RT-DANO-MES-BASE        PIC S9(006)       COMP-3.
           10  K4RT-CPROD                PIC S9(003)       COMP-3.
           10  K4RT-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
           10  K4RT-CCNPJ-CPF            PIC S9(009)       COMP-3.
           10  K4RT-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
           10  K4RT-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
           10  K4RT-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
           10  K4RT-HATULZ               PIC  X(026).
           10  K4RT-CFUNC-BDSCO          PIC S9(009)       COMP-3.
           10  K4RT-CTERM                PIC  X(008).
           10  K4RT-DINCL-REG            PIC  X(010).
           10  K4RT-CERRO-PGTO-INCOT     PIC S9(002)       COMP-3.
