      *================================================================*
      * DESCRICAO  : INCLUIR OPERACAO DE DESCONTO - BOTAO CONTRATAR    *
      * COPYBOOK   : DCOMWAME - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AMI - CONTRATACAO DA OPERACAO                *
      * FLUXO      : DCOMIABL                                          *
      * DATA       : 10/10/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA                                *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAME-E-HEADER.
             10 DCOMWAME-COD-LAYOUT    PIC X(008)  VALUE 'DCOMWAME'.
             10 DCOMWAME-TAM-LAYOUT    PIC 9(005)  VALUE 03000.
      *
          05 DCOMWAME-BLOCO-ENTRADA.
      *-->   DADOS DA PROPOSTA/SIMULACAO/OPERACAO
      *-->   =========================================
             10 DCOMWAME-CPPSTA-PRODT              PIC  9(019).
             10 DCOMWAME-NSMULA-OPER-DESC          PIC  9(009).
             10 DCOMWAME-DANO-OPER-DESC            PIC  9(004).
             10 DCOMWAME-NSEQ-OPER-DESC            PIC  9(009).
             10 DCOMWAME-DINIC-OPER-DESC           PIC  X(010).
             10 DCOMWAME-CSIT-DESC-COML            PIC  9(003).
             10 DCOMWAME-CPRODT                    PIC  9(003).
             10 DCOMWAME-CSPROD-DESC-COML          PIC  9(003).
             10 DCOMWAME-CPRODT-SERVC-OPER         PIC  9(008).
             10 DCOMWAME-CTPO-DESC-COML            PIC  9(003).
             10 DCOMWAME-CBCO                      PIC  9(003).
             10 DCOMWAME-CAG-BCRIA                 PIC  9(005).
             10 DCOMWAME-CCTA-BCRIA-CLI            PIC  9(013).
             10 DCOMWAME-CBCO-OPER-OPER            PIC  9(003).
             10 DCOMWAME-CAG-OPER-OPER             PIC  9(005).
             10 DCOMWAME-CCONTR-CONVE-DESC         PIC  9(009).
             10 DCOMWAME-CVRSAO-CONTR-CONVE        PIC  9(003).
             10 DCOMWAME-CELMTO-DESC-COML          PIC  9(003).
             10 DCOMWAME-CINDCD-DIGTC-AG           PIC  X(001).
             10 DCOMWAME-VOPER-DESC-COML           PIC  9(015)V9(002).
             10 DCOMWAME-TOPER-DESC-APURA          PIC  9(003).
             10 DCOMWAME-VOPER-DESC-APURA          PIC  9(015)V9(002).
             10 DCOMWAME-QTITLO-OPER-APURA         PIC  9(005).
             10 DCOMWAME-DVCTO-FNAL-APURA          PIC  X(010).
             10 DCOMWAME-TMED-PONDE-OPER           PIC  9(003).
             10 DCOMWAME-CMEIO-ENTRD-DESC          PIC  9(003).
             10 DCOMWAME-CTPO-TX-JURO-DESC         PIC  9(003).
             10 DCOMWAME-PALIQT-IOF-DESC           PIC  9(003)V9(005).
             10 DCOMWAME-PTX-JURO-MES              PIC  9(003)V9(005).
             10 DCOMWAME-PTX-JURO-ANO              PIC  9(003)V9(005).
             10 DCOMWAME-VJURO-OPER-DESC           PIC  9(015)V9(002).
             10 DCOMWAME-VTARIF-REG-TITLO          PIC  9(015)V9(002).
             10 DCOMWAME-VTAC-OPER-DESC            PIC  9(015)V9(002).
             10 DCOMWAME-VIOF-OPER-DESC            PIC  9(015)V9(002).
             10 DCOMWAME-VLIQ-OPER-DESC            PIC  9(015)V9(002).
             10 DCOMWAME-DINCL-REG                 PIC  X(010).
             10 DCOMWAME-PTX-CUSTO-SPREAD          PIC  9(003)V9(003).
             10 DCOMWAME-QTITLO-CHEQ-PCELA         PIC  9(009).
             10 DCOMWAME-CINDCD-ALIQT-REDZD        PIC  X(001).
             10 DCOMWAME-PTX-JURO-DIA              PIC  9(003)V9(005).
             10 DCOMWAME-CINDCD-FREQ-TX            PIC  X(002).
             10 DCOMWAME-DANO-BASE                 PIC  9(004).
             10 DCOMWAME-CDEPDC                    PIC  9(005).
             10 DCOMWAME-CSEQ-STUDO                PIC  9(009).
             10 DCOMWAME-CCART-LIM-CREDT           PIC  X(005).
             10 DCOMWAME-NSEQ-CONTR-LIM            PIC  9(017).
             10 DCOMWAME-CPSSOA-JURID-CONTR        PIC  9(010).
             10 DCOMWAME-CTPO-CONTR-NEGOC          PIC  9(003).
             10 DCOMWAME-NSEQ-CONTR-NEGOC          PIC  9(010).
             10 DCOMWAME-CTPO-CANAL                PIC  9(003).
             10 DCOMWAME-CAUTRZ-TX-TESOU           PIC  9(009).
             10 DCOMWAME-SELEC-CATAO-CREDT         PIC  X(001).
             10 DCOMWAME-PTX-CUSTO-MES             PIC  9(003)V9(006).
             10 DCOMWAME-PTX-CUSTO-ANO             PIC  9(003)V9(006).
      *
      *-->   DADOS DA TAXA/ENQUADRAMENTO
      *-->   =========================================
      *-->   INTERVALO DE TAXA DO PRODUTO (SEGURANCA)
             10 DCOMWAME-PTX-MIN-PROD-DESC         PIC  9(003)V9(003).
             10 DCOMWAME-PTX-MAX-JURO-DESC         PIC  9(003)V9(003).
             10 DCOMWAME-CINDCD-FREQ-TX-DESC       PIC  X(002).
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQUAD. PROPOSTA)
             10 DCOMWAME-PTX-MIN-SPROD-DESC        PIC  9(003)V9(003).
             10 DCOMWAME-PTX-MAX-SPROD-DESC        PIC  9(003)V9(003).
             10 DCOMWAME-CINDCD-FREQ-TX-SPROD      PIC  X(002).
      *-->   TAXA MINIMA ESPECIAL DO CLIENTE
             10 DCOMWAME-PTX-JURO-ESP-CLI          PIC  9(003)V9(003).
             10 DCOMWAME-CINDCD-FREQ-TX-CLIE       PIC  X(002).
      *
      *-->   OPCAO DO CLIENTE PARA O CALCULO DE IOF
      *-->   =========================================
      *-->   -> REDUZ IOF? "S" - REDUZIDA/ "N" - NORMAL
             10 DCOMWAME-FLAG-RED-IOF              PIC  X(001).
      *-->   -> ADICIONA IOF?  "S" - SIM / "N" - NAO
             10 DCOMWAME-FLAG-ADIC-IOF             PIC  X(001).
      *-->   -> CALCULA CUSTO? "S" - SIM / "N" - NAO
             10 DCOMWAME-FLAG-CALC-CUSTO           PIC  X(001).
      *
      *-->   DADOS COMPLEMENTARES DA OPERACAO
      *-->   =========================================
             10 DCOMWAME-CVAR-PRODT-RURAL          PIC  9(005).
             10 DCOMWAME-CNRO-REFT-BACEN           PIC  9(018).
             10 DCOMWAME-CCESTA-SAFRA-RURAL        PIC  9(005).
             10 DCOMWAME-CINDCD-ZNMTO              PIC  9(001).
             10 DCOMWAME-CTPO-BNEFC-RURAL          PIC  9(002).
             10 DCOMWAME-NBASE-LEGAL-RENEG         PIC  9(005).
             10 DCOMWAME-CREG-AMBTL-RURAL          PIC  X(041).
             10 DCOMWAME-CORIGE-ANTEC-PGMD         PIC  9(001).
             10 DCOMWAME-CCONTR-LIM-DESC           PIC  9(009).
             10 DCOMWAME-CVRSAO-CONTR-LIM          PIC  9(003).
             10 DCOMWAME-CINDCD-FORMA-LIQDC        PIC  X(001).
             10 DCOMWAME-CINDCD-UTILZ-LIM          PIC  X(001).
             10 DCOMWAME-CINDCD-DEB-PCIAL          PIC  X(001).
             10 DCOMWAME-CINDCD-TENTV-DEB          PIC  X(001).
      *
      *-->   DADOS DO CEDENTE/PROPONENTE DA OPERACAO
      *-->   =========================================
             10 DCOMWAME-CCNPJ-CPF                 PIC  9(009).
             10 DCOMWAME-CFLIAL-CNPJ               PIC  9(004).
             10 DCOMWAME-CCTRL-CNPJ-CPF            PIC  9(002).
             10 DCOMWAME-CDOCTO-ID                 PIC  X(015).
             10 DCOMWAME-IPSSOA-DESC-COML          PIC  X(060).
             10 DCOMWAME-CCLUB                     PIC  9(010).
             10 DCOMWAME-CCEP                      PIC  9(005).
             10 DCOMWAME-CCEP-COMPL                PIC  9(003).
             10 DCOMWAME-ELOGDR                    PIC  X(040).
             10 DCOMWAME-ELOGDR-NRO                PIC  X(007).
             10 DCOMWAME-CSGL-UF                   PIC  X(002).
      *
      *-->   DADOS DAS TESTEMUNHAS DA OPERACAO
      *-->   =========================================
             10 DCOMWAME-TESTEMUNHAS               OCCURS 002 TIMES.
                15 DCOMWAME-CCNPJ-CPF-TSTM         PIC  9(009).
                15 DCOMWAME-CFLIAL-CNPJ-TSTM       PIC  9(004).
                15 DCOMWAME-CCTRL-CNPJ-CPF-TSTM    PIC  9(002).
                15 DCOMWAME-CDOCTO-ID-TSTM         PIC  X(015).
                15 DCOMWAME-CCLUB-TSTM             PIC  9(010).
                15 DCOMWAME-IPSSOA-DESC-COML-TSTM  PIC  X(060).
      *
      *-->   DADOS DO MEIO DE LIBERACAO DA OPERACAO
      *-->   =========================================
             10 DCOMWAME-CMEIO-LIBRC-DESC          PIC  9(003).
             10 DCOMWAME-NOPER-MEIO-LIBRC          PIC  9(003).
             10 DCOMWAME-CBCO-LIBRC                PIC  9(003).
             10 DCOMWAME-CAG-BCRIA-LIBRC           PIC  9(005).
             10 DCOMWAME-CCTA-BCRIA-CLI-LIBRC      PIC  9(013).
             10 DCOMWAME-CCNPJ-CPF-LIBRC           PIC  9(009).
             10 DCOMWAME-CFLIAL-CNPJ-LIBRC         PIC  9(004).
             10 DCOMWAME-CCTRL-CNPJCPF-LBR         PIC  9(002).
             10 DCOMWAME-IPSSOA-DESC-LIBRC         PIC  X(060).
             10 DCOMWAME-CCLUB-LIBRC               PIC  9(010).
             10 DCOMWAME-PLIBRC-DESC-COML          PIC  9(003)V9(003).
      *
      *-->   DADOS DE CUSTO EFETIVO TOTAL DA OPERACAO
      *-->   =========================================
             10 DCOMWAME-VCUSTO-DESC-DIA           PIC  9(008)V9(007).
             10 DCOMWAME-VCUSTO-DESC-MES           PIC  9(008)V9(007).
             10 DCOMWAME-VCUSTO-DESC-ANO           PIC  9(008)V9(007).
      *
      *-->   DADOS DE PARAMETROS DO SUBPRODUTO
      *-->   =========================================
      *-->   -> PARAMETROS DO SUBPRODUTO DCOMB064 - TSPROD_MEIO_ENTRD
             10 DCOMWAME-CINDCD-LIBRC-ANTCP        PIC  X(001).
             10 DCOMWAME-CINDCD-FORML-OPER         PIC  X(001).
      *-->   -> PARAMETROS DO SUBPRODUTO DCOMB068 - TSPROD_REGRA_OPER
             10 DCOMWAME-CINDCD-PGDOR-JURO         PIC  X(001).
             10 DCOMWAME-CINDCD-CALC-JURO          PIC  X(001).
             10 DCOMWAME-CINDCD-INCID-IOF          PIC  X(001).
             10 DCOMWAME-FILLER                    PIC  X(096).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
             10 DCOMWAME-FILLER                    PIC  X(1817).
      *
