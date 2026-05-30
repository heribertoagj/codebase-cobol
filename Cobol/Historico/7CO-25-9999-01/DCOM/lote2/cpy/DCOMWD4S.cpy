      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWD4S                                          *
      *----------------------------------------------------------------*
      *    Area de saida do modulo DCOM4D4S/4D4I/4D4U                  *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMWD4S-BLOCO-SAIDA.
           05 DCOMWD4S-OCORRENCIAS OCCURS 070 TIMES INDEXED BY IND-D4S.
              10 DCOMWD4S-NPSSOA-DESC-COML   PIC  9(09).
4S2511*         10 DCOMWD4S-CCNPJ-CPF        PIC  9(09).
4S2511          10 DCOMWD4S-CCNPJ-CPF        PIC  X(09).
4S2511*         10 DCOMWD4S-CFLIAL-CNPJ      PIC  9(04).
4S2511          10 DCOMWD4S-CFLIAL-CNPJ      PIC  X(04).
              10 DCOMWD4S-CCTRL-CNPJ-CPF     PIC  9(02).
              10 DCOMWD4S-CDOCTO-ID          PIC  X(15).
              10 DCOMWD4S-IPSSOA-DESC-COML   PIC  X(60).
              10 DCOMWD4S-DINCL-REG          PIC  X(10).
              10 DCOMWD4S-HULT-ATULZ         PIC  X(26).
              10 DCOMWD4S-CFUNC-BDSCO        PIC  9(09).
              10 DCOMWD4S-CTERM              PIC  X(08).
              10 DCOMWD4S-CINDCD-TPO-PSSOA   PIC  X(01).