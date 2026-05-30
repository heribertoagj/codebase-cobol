      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQC                                      *
      * TAMANHO : 0146 BYTES                                           *
      * ARQUIVO : TIPO DE PESSOA DESCONTO COMERCIAL                    *
      * INCLUDE DA TABELA DCOMB0D4 - DB2PRD.TPSSOA_DESC_COML           *
      ******************************************************************
       01  QC-D4-REGISTRO.
 01 05     05 QC-D4-NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
 06 05     05 QC-D4-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 11 03     05 QC-D4-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 14 02     05 QC-D4-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 16 15     05 QC-D4-CDOCTO-ID            PIC X(15).
 31 60     05 QC-D4-IPSSOA-DESC-COML     PIC X(60).
 91 06     05 QC-D4-CCLUB                PIC S9(10)V USAGE COMP-3.
 97 10     05 QC-D4-DINCL-REG            PIC X(10).
107 26     05 QC-D4-HULT-ATULZ           PIC X(26).
133 05     05 QC-D4-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
138 08     05 QC-D4-CTERM                PIC X(8).
146 01     05 QC-D4-CINDCD-TPO-PSSOA     PIC X(1).
