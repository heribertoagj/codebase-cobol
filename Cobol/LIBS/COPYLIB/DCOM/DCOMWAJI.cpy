      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE CONTRATO LIMITE -LAYOUT ENTRADA*
      * COPYBOOK   : DCOMWAJI - BOOK DE ACESSO AO PROGRAMA COORDENADOR *
      * COORDENADOR: DCOM3AJL - LISTA DETALHADA DE CONTRATO LIMITE     *
      * DATA       : 03/08/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAJI-HEADER.
             10 DCOMWAJI-COD-LAYOUT  PIC X(008)      VALUE 'DCOMWAJI'.
             10 DCOMWAJI-TAM-LAYOUT  PIC 9(005)      VALUE 25297.
      *
          05 DCOMWAJI-REGISTRO.
             10 DCOMWAJI-BLOCO-SESSAO.
                15 DCOMWAJI-CCANAL                   PIC  9(003).
                15 DCOMWAJI-CAUTEN-SEGRC.
                   20 DCOMWAJI-CAUTEN-SEGRC-N        PIC  9(009).
                15 DCOMWAJI-DEPENDENCIA-OPER         PIC  9(005).
                15 DCOMWAJI-EMPR-USUAR-TRAB          PIC  9(010).
                15 DCOMWAJI-DEPEND-USUAR-TRAB        PIC  9(008).
                15 DCOMWAJI-CODIGO-DEPENDENCIA       PIC  9(005).
                15 DCOMWAJI-RESERVA                  PIC  X(060).

             10 DCOMWAJI-BLOCO-ENTRADA.
                15 DCOMWAJI-E-FUNCAO                 PIC  9(002).
                15 DCOMWAJI-E-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAJI-E-CPRODT                 PIC  9(003).
                15 DCOMWAJI-E-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAJI-E-CBCO                   PIC  9(003).
                15 DCOMWAJI-E-CAG-BCRIA              PIC  9(005).
                15 DCOMWAJI-E-CCTA-BCRIA-CLI         PIC  9(013).
                15 DCOMWAJI-E-CCNPJ-CPF              PIC  9(009).
                15 DCOMWAJI-E-CFLIAL-CNPJ            PIC  9(004).
                15 DCOMWAJI-E-CCTRL-CNPJ-CPF         PIC  9(002).
                15 DCOMWAJI-E-CCONTR-LIM-DESC        PIC  9(009).
                15 DCOMWAJI-E-CVRSAO-CONTR-LIM       PIC  9(003).
                15 DCOMWAJI-E-DANO-OPER-DESC         PIC  9(004).
                15 DCOMWAJI-E-NSEQ-OPER-DESC         PIC  9(009).
                15 DCOMWAJI-E-RESERVA                PIC  X(010).

             10 DCOMWAJI-BLOCO-PAGINACAO.
                15 DCOMWAJI-INDICADOR-PAGINACAO      PIC  X(001).
                   88 DCOMWAJI-P-INICIAL             VALUE 'I'.
                   88 DCOMWAJI-P-PRIMEIRA            VALUE 'P'.
                   88 DCOMWAJI-P-SEGUINTE            VALUE 'S'.
                   88 DCOMWAJI-P-ANTERIOR            VALUE 'A'.
                   88 DCOMWAJI-P-ULTIMA              VALUE 'U'.
                15 DCOMWAJI-CHAVE-INICIO.
                   20 DCOMWAJI-I-CCONTR-LIM-DESC     PIC  9(009).
                   20 DCOMWAJI-I-CVRSAO-CONTR-LIM    PIC  9(003).
                   20 DCOMWAJI-I-DINIC-VGCIA-CONTR   PIC  X(010).
                   20 DCOMWAJI-I-RESERVA             PIC  X(050).
                15 DCOMWAJI-CHAVE-FINAL.
                   20 DCOMWAJI-F-CCONTR-LIM-DESC     PIC  9(009).
                   20 DCOMWAJI-F-CVRSAO-CONTR-LIM    PIC  9(003).
                   20 DCOMWAJI-F-DINIC-VGCIA-CONTR   PIC  X(010).
                   20 DCOMWAJI-F-RESERVA             PIC  X(050).

             10 DCOMWAJI-BLOCO-SAIDA.
                15 DCOMWAJI-S-QTOT-CONTRATOS         PIC  9(010).
                15 DCOMWAJI-S-QTDE-CONTRATOS         PIC  9(002).
      *-->         --------------------------------------------
      *-->   --->  LISTA DETALHADA DE CONTRATO LIMITE - MAX. 05  <---
      *-->         --------------------------------------------
                15 DCOMWAJI-S-CONTRATOS              OCCURS 005 TIMES.
                   20 DCOMWAJI-S-CCONTR-LIM-DESC     PIC  9(009).
                   20 DCOMWAJI-S-CVRSAO-CONTR-LIM    PIC  9(003).
      *-->         --------------------------------------------
      *-->   --->  DADOS BASICOS DO CONTRATO LIMITE              <---
      *-->         --------------------------------------------
                   20 DCOMWAJI-S-CSIT-DESC-COML      PIC  9(003).
                   20 DCOMWAJI-S-IRSUMO-SIT-DESC     PIC  X(015).
                   20 DCOMWAJI-S-CPRODT-SERVC-OPER   PIC  9(008).
                   20 DCOMWAJI-S-CPRODT              PIC  9(003).
                   20 DCOMWAJI-S-IPRODT              PIC  X(060).
                   20 DCOMWAJI-S-CSPROD-DESC-COML    PIC  9(003).
                   20 DCOMWAJI-S-IRSUMO-SPROD-DESC   PIC  X(040).
                   20 DCOMWAJI-S-CCLUB               PIC  9(010).
                   20 DCOMWAJI-S-CCNPJ-CPF           PIC  9(009).
                   20 DCOMWAJI-S-CFLIAL-CNPJ         PIC  9(004).
                   20 DCOMWAJI-S-CCTRL-CNPJ-CPF      PIC  9(002).
                   20 DCOMWAJI-S-IPSSOA-DESC-COML    PIC  X(060).
                   20 DCOMWAJI-S-CINDCD-MODLD-DESC   PIC  X(001).
                   20 DCOMWAJI-S-CBCO                PIC  9(003).
                   20 DCOMWAJI-S-CAG-BCRIA           PIC  9(005).
                   20 DCOMWAJI-S-CCTA-BCRIA-CLI      PIC  9(013).
                   20 DCOMWAJI-S-CBCO-OPER-CONTR     PIC  9(003).
                   20 DCOMWAJI-S-CAG-OPER-CONTR      PIC  9(005).
                   20 DCOMWAJI-S-DINCL-CONTR-LIM     PIC  X(010).
                   20 DCOMWAJI-S-DINIC-VGCIA-CONTR   PIC  X(010).
                   20 DCOMWAJI-S-DVCTO-CONTR-LIM     PIC  X(010).
                   20 DCOMWAJI-S-TCONTR-LIM-DESC     PIC  9(005).
                   20 DCOMWAJI-S-CINDCD-PRZ-CONTR    PIC  X(002).
                   20 DCOMWAJI-S-VCONTR-LIM-DESC     PIC  9(015)V9(002).
                   20 DCOMWAJI-S-VUTILZ-CONTR-LIM    PIC  9(015)V9(002).
                   20 DCOMWAJI-S-VDISPN-CONTR-LIM    PIC  9(015)V9(002).
                   20 DCOMWAJI-S-CINDCD-CONTR-ROTVO  PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-RENOV-AUTOM  PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-RTEIO-BNEFC  PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-LIBRC-CONTR  PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-TARIF-ADTTO  PIC  X(001).
                   20 DCOMWAJI-S-DINIC-PARMZ-CONTR   PIC  X(010).
                   20 DCOMWAJI-S-CTPO-COBR-TAC       PIC  9(003).
                   20 DCOMWAJI-S-ITPO-COBR-TAC       PIC  X(030).
                   20 DCOMWAJI-S-CINDCD-TAC-SPROD    PIC  X(001).
                   20 DCOMWAJI-S-VTAC-CONTR-LIM      PIC  9(015)V9(002).
                   20 DCOMWAJI-S-DTAC-CONTR-LIM      PIC  X(010).
                   20 DCOMWAJI-S-DRENOV-CONTR-LIM    PIC  X(010).
                   20 DCOMWAJI-S-CGARAN              PIC  9(003).
                   20 DCOMWAJI-S-IGARAN              PIC  X(040).
                   20 DCOMWAJI-S-RPRCER-FORML-DESC   PIC  X(240).
                   20 DCOMWAJI-S-FILLER              PIC  X(100).
      *-->         ---------------------------------------------
      *-->   --->  LISTA DE CONSULTAS DE LIMITE CREDITO- MAX. 10  <---
      *-->         ---------------------------------------------
                   20 DCOMWAJI-S-QTDE-LIMCRED        PIC  9(002).
                   20 DCOMWAJI-S-LIMCRED             OCCURS 010 TIMES.
CLIM??                25 DCOMWAJI-S-NDOCTO-PARTC-LIM PIC  9(010).
CLIM??                25 DCOMWAJI-S-NSEQ-CONTR-LIM   PIC  9(017).
                      25 DCOMWAJI-S-DANO-BASE        PIC  9(004).
                      25 DCOMWAJI-S-CDEPDC           PIC  9(005).
                      25 DCOMWAJI-S-CSEQ-STUDO       PIC  9(009).
                      25 DCOMWAJI-S-CCART-LIM-CREDT  PIC  X(005).
                      25 DCOMWAJI-S-VCONS-NEGOC      PIC  9(015)V9(002).
                      25 DCOMWAJI-S-FILLER           PIC  X(100).
      *-->         ---------------------------------------------
      *-->   --->  OPTIN - AUTORIZA��O DE D�BITO                  <---
      *-->         ---------------------------------------------
                   20 DCOMWAJI-S-CINDCD-FORMA-LIQDC  PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-UTILZ-LIM    PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-DEB-PCIAL    PIC  X(001).
                   20 DCOMWAJI-S-CINDCD-TENTV-DEB    PIC  X(001).
      *-->         ---------------------------------------------
      *-->   --->  TESTEMUNHAS DO CONTRATO LIMITE      - MAX. 02  <---
      *-->         ---------------------------------------------
                   20 DCOMWAJI-S-QTDE-TEST           PIC  9(002).
                   20 DCOMWAJI-S-TESTEMUNHAS         OCCURS 002 TIMES.
                      25 DCOMWAJI-S-TEST-CCNPJ-CPF   PIC  9(009).
                      25 DCOMWAJI-S-TEST-CFLIAL-CNPJ PIC  9(004).
                      25 DCOMWAJI-S-TEST-CCTRL-CNPJ-CPF
                                                     PIC  9(002).
                      25 DCOMWAJI-S-TEST-CDOCTO-ID   PIC  X(015).
                      25 DCOMWAJI-S-TEST-IPSSOA-DESC PIC  X(060).
      *-->         ---------------------------------------------
      *-->   --->  FIADORES DO CONTRATO LIMITE         - MAX. 08  <---
      *-->         ---------------------------------------------
                   20 DCOMWAJI-S-QTDE-FIAD           PIC  9(002).
                   20 DCOMWAJI-S-FIADORES            OCCURS 008 TIMES.
                      25 DCOMWAJI-S-FIAD-TPO-PSSOA   PIC  X(001).
                      25 DCOMWAJI-S-FIAD-CCNPJ-CPF   PIC  9(009).
                      25 DCOMWAJI-S-FIAD-CFLIAL-CNPJ PIC  9(004).
                      25 DCOMWAJI-S-FIAD-CCTRL-CNPJ-CPF
                                                     PIC  9(002).
                      25 DCOMWAJI-S-FIAD-CDOCTO-ID   PIC  X(015).
                      25 DCOMWAJI-S-FIAD-IPSSOA-DESC PIC  X(060).
                      25 DCOMWAJI-S-FIAD-CBCO        PIC  9(003).
                      25 DCOMWAJI-S-FIAD-CAG-BCRIA   PIC  9(005).
                      25 DCOMWAJI-S-FIAD-CCTA-BCRIA-CLI
                                                     PIC  9(013).
                      25 DCOMWAJI-S-FIAD-ELOGDR      PIC  X(040).
                      25 DCOMWAJI-S-FIAD-IBAIRRO     PIC  X(020).
                      25 DCOMWAJI-S-FIAD-CID-ELOGDR  PIC  X(040).
                      25 DCOMWAJI-S-FIAD-CSGL-UF     PIC  X(002).
                      25 DCOMWAJI-S-FIAD-CCEP        PIC  9(005).
                      25 DCOMWAJI-S-FIAD-CCEP-COMPL  PIC  9(003).
                      25 DCOMWAJI-S-CINDCD-CONJG-AUTRZ
                                                     PIC  X(001).
                      25 DCOMWAJI-S-IPSSOA-CONJG     PIC  X(040).
                      25 DCOMWAJI-S-CCPF-CONJG       PIC  9(009).
                      25 DCOMWAJI-S-CCTRL-CPF-CONJG  PIC  9(002).
                      25 DCOMWAJI-S-CDOCTO-ID-CONJG  PIC  X(015).
