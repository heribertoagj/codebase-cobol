      *****************************************************************
      * NOME DA INC - GFCTWKX0                                        *
      * DESCRICAO   - MOVIMENTO CESTA EXCLUSIVE                       *
ST2507* TAMANHO     - 137                                             *
      * DATA        - NOV/2016                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      * =========================================================     *
      * FORMATOS:                                                     *
      * KX0-DINIC-ADSAO-INDVD - AAAA.MM.DD (ZONADO)                   *
      * KX0-DFIM-ADSAO-INDVD  - AAAA.MM.DD (ZONADO)                   *
      * KX0-DATA-ENCART       - 0AAAAMMDD  (ZONADO)                   *
      * KX0-DATA-ABERT        - 0AAAAMMDD  (PK)                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *
       01  GFCTWKX0-REG.
           10 KX0-CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
           10 KX0-CCTA-CLI             PIC S9(7)V USAGE COMP-3.
           10 KX0-CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
           10 KX0-HINCL-REG            PIC X(26).
           10 KX0-CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
           10 KX0-CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
           10 KX0-DINIC-ADSAO-INDVD    PIC X(10).
           10 KX0-DFIM-ADSAO-INDVD     PIC X(10).
           10 KX0-HMANUT-REG           PIC X(26).
           10 KX0-CINDCD-EXCL-REG      PIC S9(1)V USAGE COMP-3.
           10 KX0-CTPO-SERVC-TARIF     PIC S9(1)V USAGE COMP-3.
           10 KX0-DATA-GFCT            PIC X(10).
           10 KX0-QFRANQ-BONIF         PIC S9(03)V COMP-3.
           10 KX0-DATA-ENCART          PIC 9(09).
ST2507*    10 KX0-PRINCIPAL-CPF        PIC S9(09)V COMP-3.
ST2507*    10 KX0-FILIAL-CPF           PIC S9(05)V COMP-3.
ST2507     10 KX0-PRINCIPAL-CPF        PIC  X(09).
           10 KX0-CTR-CPF              PIC 9(02).
           10 KX0-DATA-ABERT           PIC S9(09)V COMP-3.
           10 KX0-SEG-TP-SEGMENTO      PIC S9(03)V COMP-3.
      *
      *****************************************************************
      *
