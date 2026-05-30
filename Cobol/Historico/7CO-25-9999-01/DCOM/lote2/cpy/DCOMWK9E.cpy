      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWK9E                                          *
      *----------------------------------------------------------------*
      *    Comarea de entrada para o modulo DCOM4K9F/K9I/K9D           *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumerico          *
      ******************************************************************

       01 DCOMWK9E-BLOCO-ENTRADA.
           05 DCOMWK9E-CBCO                 PIC  9(03).
           05 DCOMWK9E-CAG-BCRIA            PIC  9(05).
           05 DCOMWK9E-NPSSOA-DESC-COML     PIC  9(09).
           05 DCOMWK9E-DULT-UTILZ           PIC  X(10).
           05 DCOMWK9E-DINCL-REG            PIC  X(10).
           05 DCOMWK9E-HULT-ATULZ           PIC  X(26).
           05 DCOMWK9E-CFUNC-BDSCO          PIC  9(09).
           05 DCOMWK9E-CTERM                PIC  X(08).
           05 DCOMWK9E-INSTRUCAO            PIC  X(10).
           05 DCOMWK9E-INDICADOR-PAGINACAO  PIC  X(01).
           05 DCOMWK9E-MAX-OCORR            PIC  9(03).
4S2511*      05 DCOMWK9E-V-CCNPJ-CPF        PIC  9(09).
4S2511       05 DCOMWK9E-V-CCNPJ-CPF        PIC  X(09).
4S2511*      05 DCOMWK9E-V-CFLIAL-CNPJ      PIC  9(04).
4S2511       05 DCOMWK9E-V-CFLIAL-CNPJ      PIC  X(04).
           05 DCOMWK9E-V-CCTRL-CNPJ-CPF     PIC  9(02).

       01 DCOMWK9E-ENTRADA.
           05 DCOMWK9E-CBCO                 PIC  9(03).
           05 DCOMWK9E-CAG-BCRIA            PIC  9(05).
           05 DCOMWK9E-NPSSOA-DESC-COML     PIC  9(09).
           05 DCOMWK9E-DULT-UTILZ           PIC  X(10).
           05 DCOMWK9E-DINCL-REG            PIC  X(10).
           05 DCOMWK9E-HULT-ATULZ           PIC  X(26).
           05 DCOMWK9E-CFUNC-BDSCO          PIC  9(09).
           05 DCOMWK9E-CTERM                PIC  X(08).
           05 DCOMWK9E-INSTRUCAO            PIC  X(10).
           05 DCOMWK9E-INDICADOR-PAGINACAO  PIC  X(01).
           05 DCOMWK9E-MAX-OCORR            PIC  9(03).
4S2511*      05 DCOMWK9E-V-CCNPJ-CPF        PIC  9(09).
4S2511       05 DCOMWK9E-V-CCNPJ-CPF        PIC  X(09).
4S2511*      05 DCOMWK9E-V-CFLIAL-CNPJ      PIC  9(04).
4S2511       05 DCOMWK9E-V-CFLIAL-CNPJ      PIC  X(04).
           05 DCOMWK9E-V-CCTRL-CNPJ-CPF     PIC  9(02).