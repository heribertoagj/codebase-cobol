      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMGD         *
      * DESCRICAO : LOTE PROCESSAMENTO DIARIOS PARA REPROCESSAMENTO   *
      * TAMANHO   : LRECL = 175                                       *
      *****************************************************************
      ***********************DELETAR COMENTARIO************************
      ***  INCLUI CAMPOS DE NULIDADE - AUMENTOU PARA 175 **************
      ***  O CAMPO GD-CEMPR-INTFC-BASIL NAO EXCLUIDO     **************
      ***********************DELETAR COMENTARIO************************

       01  DCOMGD-AREA.

  1  7     05  GD-NSEQ-REG-MOVTO        PIC S9(012)       COMP-3.
  8 10     05  GD-DPROCM-MOVTO-BASIL    PIC  X(010).
 18  1     05  GD-CRPROC                PIC  X(001).
 19  1     05  GD-CTPO-LYOUT-CABEC      PIC  X(001).
 20  1     05  GD-CTPO-LYOUT-CABEC-NULL PIC  X(001).
 21  4     05  GD-CCTRO-CUSTO           PIC  X(004).
 25  1     05  GD-CCTRO-CUSTO-NULL      PIC  X(001).
 26  7     05  GD-NSEQ-REG-CABEC        PIC S9(012)       COMP-3.
 33 32     05  GD-CERRO-REG-CABEC       PIC  X(032).
 65  5     05  GD-CCNPJ-CPF             PIC S9(009)       COMP-3.
 70  3     05  GD-CFLIAL-CNPJ           PIC S9(005)       COMP-3.
 73  2     05  GD-CCTRL-CNPJ-CPF        PIC S9(002)       COMP-3.
 75  1     05  GD-CTPO-LYOUT-TRAIL      PIC  X(001).
 76  5     05  GD-QREG-DETLH-MOVTO      PIC S9(009)       COMP-3.
 81  1     05  GD-QREG-DETLH-MOVTO-NULL PIC  X(001).
 82  5     05  GD-QREG-TOT-MOVTO        PIC S9(009)       COMP-3.
 87  7     05  GD-NSEQ-REG-RODAP        PIC S9(012)       COMP-3.
 94 32     05  GD-CERRO-REG-RODAP       PIC  X(032).
126 26     05  GD-HATULZ                PIC  X(026).
152  5     05  GD-CFUNC-BDSCO           PIC S9(009)       COMP-3.
157  1     05  GD-CFUNC-BDSCO-NULL      PIC  X(001).
158  8     05  GD-CTERM                 PIC  X(008).
166 10     05  GD-DINCL-REG             PIC  X(010).

