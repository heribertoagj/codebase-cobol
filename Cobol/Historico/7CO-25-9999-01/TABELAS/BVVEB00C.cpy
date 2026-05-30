      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPDIDO_RVSAO_CNSTT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB00C))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB00C)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPDIDO_RVSAO_CNSTT TABLE
           ( NPDIDO_RVSAO_OPER              DECIMAL(9, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0),
             CAG_BCRIA                      DECIMAL(5, 0),
             CCTA_BCRIA_CLI                 DECIMAL(13, 0),
             CCNPJ_TTLAR_RCBVL              DECIMAL(9, 0) NOT NULL,
             CFLIAL_TTLAR_RCBVL             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_RCBVL               DECIMAL(2, 0) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20) NOT NULL,
             COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             COPER_RVSAO_CNSTT              CHAR(20) NOT NULL,
             CCRDRA_PDIDO_RVSAO             DECIMAL(4, 0) NOT NULL,
             CCNPJ_ENTID_REG                DECIMAL(9, 0) NOT NULL,
             CFLIAL_ENTID_REG               DECIMAL(4, 0) NOT NULL,
             CCTRL_ENTID_REG                DECIMAL(2, 0) NOT NULL,
             CSIT_PDIDO_RVSAO               CHAR(1) NOT NULL,
             CSIT_RETOR_RVSAO               CHAR(1),
             HRECEB_PDIDO_RVSAO             TIMESTAMP NOT NULL,
             HRETOR_MSGEM_CIPAG             TIMESTAMP,
             HENVIO_MSGEM_CIPAG             TIMESTAMP,
             CUSUAR_RESP                    CHAR(9),
             RJUSTF_PDIDO_RVSAO             CHAR(250),
             CIDTFD_ENVIO_EMAIL             CHAR(1) NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL_ST           CHAR(9) NOT NULL,
             CFLIAL_TTLAR_RCBVL_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_RCBVL_ST            DECIMAL(2, 0) NOT NULL,
             CCNPJ_ENTID_REG_ST             CHAR(9) NOT NULL,
             CFLIAL_ENTID_REG_ST            CHAR(4) NOT NULL,
             CCTRL_ENTID_REG_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPDIDO_RVSAO_CNSTT          *
      ******************************************************************
       01  BVVEB00C.
      *    *************************************************************
           10 NPDIDO-RVSAO-OPER    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-RVSAO-CNSTT    PIC X(20).
      *    *************************************************************
           10 CCRDRA-PDIDO-RVSAO   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-REG      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-ENTID-REG     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-ENTID-REG      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PDIDO-RVSAO     PIC X(1).
      *    *************************************************************
           10 CSIT-RETOR-RVSAO     PIC X(1).
      *    *************************************************************
           10 HRECEB-PDIDO-RVSAO   PIC X(26).
      *    *************************************************************
           10 HRETOR-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 HENVIO-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      *    *************************************************************
           10 RJUSTF-PDIDO-RVSAO   PIC X(250).
      *    *************************************************************
           10 CIDTFD-ENVIO-EMAIL   PIC X(1).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL-ST
              PIC X(9).
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL-ST  PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-REG-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ENTID-REG-ST  PIC X(4).
      *    *************************************************************
           10 CCTRL-ENTID-REG-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
