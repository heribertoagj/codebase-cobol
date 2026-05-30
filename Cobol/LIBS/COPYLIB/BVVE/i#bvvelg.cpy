      *================================================================*
      * DESCRICAO    : LAYOUT PARA ARQUIVO DE SOLICITACAO DE           *
      *                AUTORIZACAO DE AGENDA                           *
      *                I#BVVELG                                        *
      *                                                                *
      * TAMANHO DO REGISTRO EM BYTES..:  200 BYTES                     *
      *                                                                *
      * DATA DE CRIACAO...............:  JUNHO/2019                    *
      * ANALISTA RESPONSAVEL..........:  PRIME INFORMATICA             *
      * PROGRAMADOR...................:  PRIME INFORMATICA             *
      *================================================================*

       01  BVVELG-REG-MOVSOLIC.
           05 BVVELG-NCADTO-AUTRZ-AGNDA   PIC  9(13).
           05 BVVELG-CCNPJ-CPF-AUTRZ      PIC  9(09).
           05 BVVELG-CFLIAL-CNPJ-AUTRZ    PIC  9(05).
           05 BVVELG-CCTRL-CPF-AUTRZ      PIC  9(02).
           05 BVVELG-CBCO                 PIC  9(03).
           05 BVVELG-CAG-BCRIA            PIC  9(05).
           05 BVVELG-CCTA-BCRIA-CLI       PIC  9(13).
           05 BVVELG-CCRDRA-ESTBL-COML    PIC  9(04).
           05 BVVELG-CTPO-VISAO-BANDE     PIC  9(03).
           05 BVVELG-CBANDE-VISAO         PIC  9(03).
           05 BVVELG-DCADTO-AUTRZ-AGNDA   PIC  X(10).
           05 BVVELG-CTPO-CANAL-CADTO     PIC  9(03).
           05 BVVELG-CTPO-ACAO-AUTRZ      PIC  X(01).
           05 BVVELG-CINDCD-DOMCL-BCRIO   PIC  X(01).
           05 BVVELG-CINDCD-CNPJ-BASE     PIC  X(01).
           05 BVVELG-CINDCD-CADTO-PRODT   PIC  X(01).
           05 BVVELG-CSIT-CADTO-SOLTC     PIC  9(01).
           05 BVVELG-CUSUAR-INCL          PIC  X(09).
           05 BVVELG-HINCL-REG            PIC  X(26).
           05 BVVELG-CUSUAR-MANUT         PIC  X(07).
           05 BVVELG-HMANUT-REG           PIC  X(26).
LAR        05 BVVELG-CID-TPO-PSSOA        PIC  X(01).
           05 BVVELG-FILLER               PIC  X(53).
