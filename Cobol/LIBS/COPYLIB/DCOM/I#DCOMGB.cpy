      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMGB         *
      * DESCRICAO : PAGAMENTOS INCONSISTIDOS PELO SISTEMA GBEL        *
      * TAMANHO   : LRECL = 148                                       *
      *****************************************************************
      ******* DELETAR COMENTARIO **************************************
      ***  INCLUI CAMPO  DE NULIDADE - AUMENTOU PARA 148 **************
      ******* DELETAR COMENTARIO **************************************

       01  DCOMGB-AREA.

  1  7     05  GB-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
  8 10     05  GB-DPROCM-PGTO-INCOT    PIC  X(010).
 18  2     05  GB-CBCO                 PIC S9(003)       COMP-3.
 20  3     05  GB-CAG-BCRIA            PIC S9(005)       COMP-3.
 23  7     05  GB-CCTA-BCRIA-CLI       PIC S9(013)       COMP-3.
 30  5     05  GB-CCART                PIC  X(005).
 35  4     05  GB-CCONTR-CORP-PCELA    PIC S9(007)       COMP-3.
 39  1     05  GB-CSIT-PGTO-INCOT      PIC S9(001)       COMP-3.
 40 10     05  GB-DVCTO-PCELA-DESC     PIC  X(010).
 50 10     05  GB-DEFETV-BAIXA-PCELA   PIC  X(010).
 60  9     05  GB-VEFETV-BAIXA-PCELA   PIC S9(015)V99    COMP-3.
 69  2     05  GB-CTPO-PGTO-CONTR      PIC S9(002)       COMP-3.
 71  2     05  GB-CTPO-PGTO-PCELA      PIC S9(002)       COMP-3.
 73  4     05  GB-DANO-MES-BASE        PIC S9(006)       COMP-3.
 77  2     05  GB-CPROD                PIC S9(003)       COMP-3.
 79  3     05  GB-NPCELA-DESC-COML     PIC S9(005)       COMP-3.
 82  5     05  GB-CCNPJ-CPF            PIC S9(009)       COMP-3.
 87  3     05  GB-CFLIAL-CNPJ          PIC S9(005)       COMP-3.
 90  2     05  GB-CCTRL-CNPJ-CPF       PIC S9(002)       COMP-3.
 92  7     05  GB-NSEQ-REG-DETLH       PIC S9(012)       COMP-3.
 99 26     05  GB-HATULZ               PIC  X(026).
125  5     05  GB-CFUNC-BDSCO          PIC S9(009)       COMP-3.
130  1     05  GB-CFUNC-NULL           PIC  X(001).
131  8     05  GB-CTERM                PIC  X(008).
139 10     05  GB-DINCL-REG            PIC  X(010).


