      ******************************************************************
      * NOME BOOK : DCOMW25J - BOOK DE INTERFACE COM SERVICO DCOM424C  *
      * DATA      : 19/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 10703 BYTES                                        *
      ******************************************************************
           05 DCOMW25J-HEADER.
             10 DCOMW25J-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25J'.
             10 DCOMW25J-TAM-LAYOUT      PIC 9(05) VALUE 10703.
           05 DCOMW25J-REGISTRO.
             10 DCOMW25J-AREA-ENTRADA.
                15 DCOMW25J-E-MAX-OCORR               PIC 9(003).
                15 DCOMW25J-E-CPRODT                  PIC 9(003).
                15 DCOMW25J-E-CSPROD-DESC-COML        PIC 9(003).
                15 DCOMW25J-E-CMEIO-ENTRD-DESC        PIC 9(003).
             10 DCOMW25J-AREA-PAGINACAO.
                15 DCOMW25J-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW25J-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW25J-I-INICIAL              VALUE 'I'.
                   88 DCOMW25J-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW25J-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW25J-I-ULTIMA               VALUE 'U'.
                15 DCOMW25J-BLOCO-RESTART.
                   20 DCOMW25J-PRI-CHAVE.
                      25 DCOMW25J-P-CPRODT            PIC 9(003).
                      25 DCOMW25J-P-CSPROD-DESC-COML  PIC 9(003).
                      25 DCOMW25J-P-CMEIO-ENTRD-DESC  PIC 9(003).
                      25 DCOMW25J-P-CIDTFD-PRODT-COBR PIC 9(002).
                      25 DCOMW25J-P-HINIC-VGCIA-DESC  PIC X(026).
                   20 DCOMW25J-ULT-CHAVE.
                      25 DCOMW25J-U-CPRODT            PIC 9(003).
                      25 DCOMW25J-U-CSPROD-DESC-COML  PIC 9(003).
                      25 DCOMW25J-U-CMEIO-ENTRD-DESC  PIC 9(003).
                      25 DCOMW25J-U-CIDTFD-PRODT-COBR PIC 9(002).
                      25 DCOMW25J-U-HINIC-VGCIA-DESC  PIC X(026).
             10 DCOMW25J-AREA-SAIDA.
                15 DCOMW25J-S-TOT-REG                 PIC  9(003).
                15 DCOMW25J-S-LISTA-SAIDA    OCCURS 100 TIMES.
                   25 DCOMW25J-S-CPRODT               PIC 9(003).
                   25 DCOMW25J-S-CSPROD-DESC-COML     PIC 9(003).
                   25 DCOMW25J-S-CMEIO-ENTRD-DESC     PIC 9(003).
                   25 DCOMW25J-S-CIDTFD-PRODT-COBR    PIC 9(002).
                   25 DCOMW25J-S-HINIC-VGCIA-DESC     PIC X(026).
                   25 DCOMW25J-S-HFIM-VGCIA-DESC      PIC X(026).
                   25 DCOMW25J-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW25J-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW25J-S-CTERM                PIC X(008).
