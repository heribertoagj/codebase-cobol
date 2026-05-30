      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMGA         *
      * DESCRICAO : PAGAMENTOS CORRIGIDOS PELO SISTEMA DCOM           *
      * TAMANHO   : LRECL = 150                                       *
      *****************************************************************
      ***  INCLUI CAMPOS DE NULIDADE - AUMENTOU PARA 150 **************
      *****************************************************************

       01  DCOMGA-AREA.

  1  7     05  GA-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
  8 10     05  GA-DPROCM-PGTO-INCOT    PIC  X(010).
 18  2     05  GA-CBCO                 PIC S9(003)       COMP-3.
 20  1     05  GA-CBCO-NULL            PIC  X(001).
 21  3     05  GA-CAG-BCRIA            PIC S9(005)       COMP-3.
 24  1     05  GA-CAG-BCRIA-NULL       PIC  X(001).
 25  7     05  GA-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
 32  5     05  GA-CCART                PIC  X(005).
 37  1     05  GA-CCART-NULL           PIC  X(001).
 38  4     05  GA-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
 42  1     05  GA-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
 43 10     05  GA-DVCTO-PCELA-DESC     PIC  X(010).
 53 10     05  GA-DEFETV-BAIXA-PCELA   PIC  X(010).
 63  9     05  GA-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
 72  2     05  GA-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
 74  2     05  GA-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
 76  4     05  GA-DANO-MES-BASE        PIC S9(006)       COMP-3.
 80  2     05  GA-CPROD                PIC S9(003)       COMP-3.
 82  3     05  GA-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
 85  5     05  GA-CCNPJ-CPF            PIC S9(009)       COMP-3.
 90  3     05  GA-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
 93  2     05  GA-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
 95  7     05  GA-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
102 26     05  GA-HATULZ               PIC  X(026).
128  5     05  GA-CFUNC-BDSCO          PIC S9(009)       COMP-3.
133  8     05  GA-CTERM                PIC  X(008).
141 10     05  GA-DINCL-REG            PIC  X(010).
