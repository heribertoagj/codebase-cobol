      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQE                                      *
      * TAMANHO : 0233 BYTES                                           *
      * ARQUIVO : RELACAO PESSOA DESCONTO COMERCIAL                    *
      * INCLUDE DA TABELA DCOMB0D6 - DB2PRD.TRELAC_PSSOA_DESC          *
      ******************************************************************
       01  QE-D6-REGISTRO.
 01 05     05 QE-D6-NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
 06 05     05 QE-D6-NRELAC-PSSOA-DESC    PIC S9(9)V USAGE COMP-3.
 11 02     05 QE-D6-CINDCD-PSSOA-DESC    PIC X(2).
 13 02     05 QE-D6-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
 15 45     05 QE-D6-CCHAVE-ELMTO-DESC    PIC X(45).
 60 02     05 QE-D6-CBCO                 PIC S9(3)V USAGE COMP-3.
 62 03     05 QE-D6-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
 65 07     05 QE-D6-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 72 02     05 QE-D6-CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
 74 40     05 QE-D6-IPSSOA-CONJG         PIC X(40).
114 05     05 QE-D6-CCPF-CONJG           PIC S9(9)V USAGE COMP-3.
119 02     05 QE-D6-CCTRL-CPF-CONJG      PIC S9(2)V USAGE COMP-3.
121 15     05 QE-D6-CDOCTO-ID-CONJG      PIC X(15).
136 01     05 QE-D6-CDOCTO-ID-NULL       PIC X(01).
137 01     05 QE-D6-CINDCD-CONJG-AUTRZ   PIC X(1).
138 03     05 QE-D6-CCEP                 PIC S9(5)V USAGE COMP-3.
141 02     05 QE-D6-CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
143 40     05 QE-D6-ELOGDR               PIC X(40).
183 02     05 QE-D6-CSGL-UF              PIC X(2).
185 10     05 QE-D6-DINCL-REG            PIC X(10).
195 26     05 QE-D6-HULT-ATULZ           PIC X(26).
221 05     05 QE-D6-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
226 08     05 QE-D6-CTERM                PIC X(8).
