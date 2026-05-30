      ******************************************************************
      * NOME BOOK : DCOMW24A - BOOK DE INTERFACE COM SERVICO DCOM424A  *
      * DESCRICAO : CONSULTA INFORMACOES DO CONTRATO                   *
      * DATA      : 02/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 452 BYTES                                          *
      *----------------------------------------------------------------*
      * DCOMW24A-E-TPO-PESQUISA = 1 - PESQUISA CHAVE COMPLETA (TOTAL)  *
      *                           2 - PESQUISA POR CONTRATO E SITUACAO *
      *                               (PARCIAL-02)                     *
      *                           3 - PESQUISA POR CONTRATO E SITUACAO *
      *                               DIFERENTE DA INFORMADA           *
      *                               (PARCIAL-03)                     *
      *                           4 - PESQUISA POR CONTRATO(PARCIAL-04)*
      ******************************************************************
       05 DCOMW24A-HEADER.
         10 DCOMW24A-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW24A'.
         10 DCOMW24A-TAM-LAYOUT        PIC 9(05)   VALUE 452.
       05 DCOMW24A-REGISTRO.
         10 DCOMW24A-BLOCO-ENTRADA.
           15 DCOMW24A-E-TPO-PESQUISA              PIC 9(01).
           15 DCOMW24A-E-CCONTR-LIM-DESC           PIC 9(09).
           15 DCOMW24A-E-CVRSAO-CONTR-LIM          PIC 9(03).
           15 DCOMW24A-E-CSIT-DESC-COML            PIC 9(03).
         10 DCOMW24A-BLOCO-SAIDA.
           15 DCOMW24A-S-CCONTR-LIM-DESC           PIC 9(09).
           15 DCOMW24A-S-CVRSAO-CONTR-LIM          PIC 9(03).
           15 DCOMW24A-S-CINDCD-MODLD-DESC         PIC X(01).
           15 DCOMW24A-S-CBCO                      PIC 9(03).
           15 DCOMW24A-S-CAG-BCRIA                 PIC 9(05).
           15 DCOMW24A-S-CCTA-BCRIA-CLI            PIC 9(13).
           15 DCOMW24A-S-CPOSTO-SERVC              PIC 9(05).
           15 DCOMW24A-S-CBCO-OPER-CONTR           PIC 9(03).
           15 DCOMW24A-S-CAG-OPER-CONTR            PIC 9(05).
           15 DCOMW24A-S-DINCL-CONTR-LIM           PIC X(10).
           15 DCOMW24A-S-DINIC-VGCIA-CONTR         PIC X(10).
           15 DCOMW24A-S-DVCTO-CONTR-LIM           PIC X(10).
           15 DCOMW24A-S-TCONTR-LIM-DESC           PIC 9(05).
           15 DCOMW24A-S-CINDCD-PRZ-CONTR          PIC X(02).
           15 DCOMW24A-S-VCONTR-LIM-DESC           PIC 9(15)V9(2).
           15 DCOMW24A-S-VUTLZD-CONTR-LIM          PIC 9(15)V9(2).
           15 DCOMW24A-S-VDISPN-CONTR-LIM          PIC 9(15)V9(2).
           15 DCOMW24A-S-HATULZ-SDO-CONTR          PIC X(26).
           15 DCOMW24A-S-CINDCD-CONTR-ROTVO        PIC X(01).
           15 DCOMW24A-S-CINDCD-RENOV-AUTOM        PIC X(01).
           15 DCOMW24A-S-CINDCD-RTEIO-BNEFC        PIC X(01).
           15 DCOMW24A-S-CINDCD-LIBRC-CONTR        PIC X(01).
           15 DCOMW24A-S-CINDCD-TARIF-ADTTO        PIC X(01).
           15 DCOMW24A-S-CPRODT                    PIC 9(03).
           15 DCOMW24A-S-DINIC-PARMZ-CONTR         PIC X(10).
           15 DCOMW24A-S-CTPO-COBR-TAC             PIC 9(03).
           15 DCOMW24A-S-CINDCD-TAC-SPROD          PIC X(01).
           15 DCOMW24A-S-VTAC-CONTR-LIM            PIC 9(15)V9(2).
           15 DCOMW24A-S-DTAC-CONTR-LIM            PIC X(10).
           15 DCOMW24A-S-CINDCD-TPO-RENOV          PIC X(01).
           15 DCOMW24A-S-DRENOV-CONTR-LIM          PIC X(10).
           15 DCOMW24A-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW24A-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW24A-S-DULT-UTILZ-CONTR          PIC X(10).
           15 DCOMW24A-S-DULT-ADTTO-CONTR          PIC X(10).
           15 DCOMW24A-S-CINDCD-MOTVO-ADTTO        PIC X(01).
           15 DCOMW24A-S-VTARIF-ADTTO-CONTR        PIC 9(15)V9(2).
           15 DCOMW24A-S-DCOBR-TARIF-ADTTO         PIC X(10).
           15 DCOMW24A-S-CCNPJ-CPF                 PIC 9(09).
           15 DCOMW24A-S-CFLIAL-CNPJ               PIC 9(05).
           15 DCOMW24A-S-CCTRL-CNPJ-CPF            PIC 9(02).
           15 DCOMW24A-S-CCLUB                     PIC 9(10).
           15 DCOMW24A-S-HULT-ATULZ                PIC X(26).
           15 DCOMW24A-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW24A-S-CTERM                     PIC X(08).
           15 DCOMW24A-S-CINDCD-OPER-INADP         PIC X(01).
           15 DCOMW24A-S-DATULZ-OPER-INADP         PIC X(10).
           15 DCOMW24A-S-CCHAVE-ELMTO-DESC         PIC X(45).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
