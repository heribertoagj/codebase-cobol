      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQW                                      *
      * TAMANHO : 0289 BYTES                                           *
      * ARQUIVO : HISTORICO PESSOA DESCONTO COMERCIAL                  *
      * INCLUDE DA TABELA DCOMB0I8 - DB2PRD.THIST_PSSOA_OPER           *
      ******************************************************************
       01  QW-I8-REGISTRO.
  1  3     05 QW-I8-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5     05 QW-I8-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3     05 QW-I8-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12 26     05 QW-I8-HULT-ATULZ           PIC X(26).
 38  5     05 QW-I8-NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
 43  5     05 QW-I8-NRELAC-PSSOA-DESC    PIC S9(9)V USAGE COMP-3.
 48  5     05 QW-I8-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 53  3     05 QW-I8-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 56  2     05 QW-I8-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 58 15     05 QW-I8-CDOCTO-ID            PIC X(15).
 73 60     05 QW-I8-IPSSOA-DESC-COML     PIC X(60).
133  6     05 QW-I8-CCLUB                PIC S9(10)V USAGE COMP-3.
139  2     05 QW-I8-CINDCD-PSSOA-DESC    PIC X(2).
141  2     05 QW-I8-CBCO                 PIC S9(3)V USAGE COMP-3.
143  3     05 QW-I8-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
146  7     05 QW-I8-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
153  3     05 QW-I8-CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
156 40     05 QW-I8-IPSSOA-CONJG         PIC X(40).
196  5     05 QW-I8-CCPF-CONJG           PIC S9(9)V USAGE COMP-3.
201  2     05 QW-I8-CCTRL-CPF-CONJG      PIC S9(2)V USAGE COMP-3.
203 15     05 QW-I8-CDOCTO-ID-CONJG      PIC X(15).
218  1     05 QW-I8-CDOCTO-ID-NULL       PIC X(1).
219  1     05 QW-I8-CINDCD-CONJG-AUTRZ   PIC X(1).
220  3     05 QW-I8-CCEP                 PIC S9(5)V USAGE COMP-3.
223  2     05 QW-I8-CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
225 40     05 QW-I8-ELOGDR               PIC X(40).
265  2     05 QW-I8-CSGL-UF              PIC X(2).
267 10     05 QW-I8-DINCL-REG            PIC X(10).
277  5     05 QW-I8-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
282  8     05 QW-I8-CTERM                PIC X(8).
