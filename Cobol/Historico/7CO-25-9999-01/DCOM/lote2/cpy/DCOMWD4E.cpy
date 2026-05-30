      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWD4E                                          *
      *----------------------------------------------------------------*
      *    Comarea de entrada para DCOM4D4S/4D4I/4D4U                  *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumerico          *
      ******************************************************************

       01 DCOMWD4E-BLOCO-ENTRADA.
           05 DCOMWD4E-NPSSOA-DESC-COML   PIC  9(09).
4S2511*      05 DCOMWD4E-CCNPJ-CPF        PIC  9(09).
4S2511       05 DCOMWD4E-CCNPJ-CPF        PIC  X(09).
4S2511*      05 DCOMWD4E-CFLIAL-CNPJ      PIC  9(04).
4S2511       05 DCOMWD4E-CFLIAL-CNPJ      PIC  X(04).
              05 DCOMWD4E-CCTRL-CNPJ-CPF  PIC  9(02).
              05 DCOMWD4E-CDOCTO-ID       PIC  X(15).
              05 DCOMWD4E-IPSSOA-DESC-COML PIC X(60).
              05 DCOMWD4E-CCLUB           PIC  9(03).
              05 DCOMWD4E-DINCL-REG       PIC  X(10).
              05 DCOMWD4E-HULT-ATULZ      PIC  X(26).
              05 DCOMWD4E-CFUNC-BDSCO     PIC  9(09).
              05 DCOMWD4E-CTERM           PIC  X(08).
              05 DCOMWD4E-CINDCD-TPO-PSSOA PIC X(01).
              05 DCOMWD4E-INSTRUCAO       PIC  X(10).

       01 DCOMWD4E-ENTRADA.
           05 DCOMWD4E-NPSSOA-DESC-COML   PIC  9(09).
4S2511*      05 DCOMWD4E-CCNPJ-CPF        PIC  9(09).
4S2511       05 DCOMWD4E-CCNPJ-CPF        PIC  X(09).
4S2511*      05 DCOMWD4E-CFLIAL-CNPJ      PIC  9(04).
4S2511       05 DCOMWD4E-CFLIAL-CNPJ      PIC  X(04).
              05 DCOMWD4E-CCTRL-CNPJ-CPF  PIC  9(02).
              05 DCOMWD4E-CDOCTO-ID       PIC  X(15).
              05 DCOMWD4E-IPSSOA-DESC-COML PIC X(60).
              05 DCOMWD4E-CCLUB           PIC  9(03).
              05 DCOMWD4E-DINCL-REG       PIC  X(10).
              05 DCOMWD4E-HULT-ATULZ      PIC  X(26).
              05 DCOMWD4E-CFUNC-BDSCO     PIC  9(09).
              05 DCOMWD4E-CTERM           PIC  X(08).
              05 DCOMWD4E-CINDCD-TPO-PSSOA PIC X(01).
              05 DCOMWD4E-INSTRUCAO       PIC  X(10).