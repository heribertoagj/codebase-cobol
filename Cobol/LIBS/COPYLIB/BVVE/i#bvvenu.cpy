      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO                            *
      *  ARQUIVO : HPU DATABELA BVVEB046 MAIS CAMPOS DA BVVEB044,      *
      *            BVVEB040 E BCPPB075                                 *
      *  LRECL   : 261 (FB)                                            *
      *  NOME INC: I#BVVENU                                            *
      *  DATA    : 10/2020                                             *
      *----------------------------------------------------------------*
       01  BVVENU-REG.
           10 BVVENU-NESTBL-AUTRZ-AGNDA   PIC S9(9)V USAGE COMP-3.
           10 BVVENU-CCNPJ-CPF-AUTRZ      PIC S9(9)V USAGE COMP-3.
           10 BVVENU-CFLIAL-CNPJ-AUTRZ    PIC S9(5)V USAGE COMP-3.
           10 BVVENU-CCTRL-CPF-AUTRZ      PIC S9(2)V USAGE COMP-3.
           10 BVVENU-CBCO                 PIC S9(3)V USAGE COMP-3.
           10 BVVENU-CBCO-NULL            PIC X(1).
           10 BVVENU-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
           10 BVVENU-CAG-BCRIA-NULL       PIC X(1).
           10 BVVENU-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
           10 BVVENU-CCTA-BCRIA-CLI-NULL  PIC X(1).
           10 BVVENU-CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
           10 BVVENU-CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
           10 BVVENU-CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
           10 BVVENU-CID-TPO-PSSOA        PIC X(1).
           10 BVVENU-CINDCD-DOMCL-BCRIO   PIC X(1).
           10 BVVENU-NCTRL-AUTRZ-AGNDA    PIC S9(11)V USAGE COMP-3.
           10 BVVENU-NCTRL-AUTRZ-AGN-NULL PIC X(1).
           10 BVVENU-CUSUAR-INCL          PIC X(9).
           10 BVVENU-HINCL-REG            PIC X(26).
           10 BVVENU-CUSUAR-MANUT         PIC X(7).
           10 BVVENU-CUSUAR-MANUT-NULL    PIC X(1).
           10 BVVENU-HMANUT-REG           PIC X(26).
           10 BVVENU-HMANUT-REG-NULL      PIC X(1).
           10 BVVENU-DASS-AUTRZ-AGNDA     PIC X(10).
           10 BVVENU-DINIC-AUTRZ-AGNDA    PIC X(10).
           10 BVVENU-DFIM-AUTRZ-AGNDA     PIC X(10).
           10 BVVENU-CBANDE-VISAO-EXTER   PIC X(5).
           10 BVVENU-CBANDE-VIS-EXT-NULL  PIC X(1).
           10 BVVENU-ICRDRA-ESTBL-COML    PIC X(50).
           10 BVVENU-ICRDRA-ESTBL-NULL    PIC X(1).
           10 BVVENU-RBANDE-VISAO         PIC X(60).
           10 BVVENU-RBANDE-VISAO-NULL    PIC X(1).
