      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWABE                                           *
      *----------------------------------------------------------------*
      *    Bloco de entrada para módulos DCOM                           *
      *    Ajuste de CNPJ/CPF e FILIAL para formato alfanumérico        *
      ******************************************************************

        01 DCOMWABE-BLOCO-ENTRADA.
           05 DCOMWABE-COD-LAYOUT        PIC  X(08).
           05 DCOMWABE-CTERM             PIC  X(08).
           05 DCOMWABE-CBCO              PIC  9(03).
           05 DCOMWABE-CAG-BCRIA         PIC  9(05).
           05 DCOMWABE-CCTA-BCRIA-CLI    PIC  9(13).
4S2511*    05 DCOMWABE-CCNPJ-CPF         PIC  9(09).
4S2511     05 DCOMWABE-CCNPJ-CPF         PIC  X(09).
4S2511*    05 DCOMWABE-CFLIAL-CNPJ       PIC  9(04).
4S2511     05 DCOMWABE-CFLIAL-CNPJ       PIC  X(04).
           05 DCOMWABE-CCTRL-CNPJ-CPF    PIC  9(02).
           05 DCOMWABE-CTPO-DESC-COML    PIC  9(03).
           05 DCOMWABE-RESERVA           PIC  X(30).