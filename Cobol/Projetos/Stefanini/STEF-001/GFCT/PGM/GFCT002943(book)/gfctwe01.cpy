      *
      ******************************************************************
      * NOME DA INC - GFCTWE01                                         *
      * DESCRICAO   - ARQUIVO DE COMUNICACAO GFCT E TRPN (OUTPUT)      *
      *               FLEXIBILIZACOES CADASTRADAS PARA OS CLIENTES     *
ST2507* TAMANHO     - 145                                              *
      * DATA        - NOV/2014                                         *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                              *
      ******************************************************************
      *
      *  WE01-PERIOD-COBR-TARIF - PERIODICIDADE DE COBRANCA DA TARIFA:
      *  D - DIARIO
      *  S - SEMANAL
      *  Q - QUINZENAL
      *  M - MENSAL
      *
      *  ESSE CAMPO NAO EXISTE MAIS. FOI SUBSTITUIDO POR:
      *
      *  WE01-PER-CBR-TARIF-1: PARA FLEX. POR AG/CTA (BASE GFCTB058)
      *                        CAMPO CPER_COBR_TARIF
      *
      *  WE01-PER-CBR-TARIF-2: PARA FLEX. POR CNPJ   (BASE GFCTB049)
      *                        CAMPO CPER_COBR_TARIF
      *
      *  WE01-CINDCD-FLEXZ-1 E WE01-CINDCD-FLEXZ-2 - INDICA SE EXISTE
      *                        FLEXIBILIZACAO CADASTRADA (S/N)
      *
      *  WE01-COD-RETORNO
      *
      *  00 - CLIENTE NAO POSSUI FLEXIBILIZACAO CADASTRADA
      *  01 - CLIENTE POSSUI UMA FLEXIBILIZACAO CADASTRADA
      *  02 - CLIENTE POSSUI DUAS FLEXIBILIZACOES CADASTRADAS
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWE01-REG.
           05 GFCTWE01-INPUT.
              10 WE01-COD-CLIENTE-TRPN     PIC  9(009).
ST2507*       10 WE01-CPF-CNPJ-PRINC       PIC  9(009).
ST2507*       10 WE01-CPF-CNPJ-FILIAL      PIC  9(005).
ST2507        10 WE01-CPF-CNPJ-PRINC       PIC  X(009).
ST2507        10 WE01-CPF-CNPJ-FILIAL      PIC  X(004).
              10 WE01-CPF-CNPJ-CONTR       PIC  9(003).
              10 WE01-CAG-BCRIA            PIC  9(005).
              10 WE01-CCTA-BCRIA           PIC  9(013).
              10 WE01-CSERVC-TARIF-TRPN    PIC  9(009).
              10 WE01-CSERVC-TARIF-GFCT    PIC  9(005).
           05 GFCTWE01-OUTPUT.
              10 WE01-DATA-BASE            PIC  X(010).
              10 WE01-VTARIF-BRUTO         PIC  9(011)V9(02).
              10 WE01-CTARIF-PCOTE         PIC  9(005).
              10 WE01-COD-RETORNO          PIC  9(002).
              10 WE01-FLEXIB-AG-CTA.
                 15 WE01-CINDCD-FLEXZ-1    PIC  X(001).
                 15 WE01-PER-CBR-TARIF-1   PIC  X(001).
                 15 WE01-QFRANQ-FLEXZ-1    PIC  9(003).
                 15 WE01-VNEGOC-FLEXZ-1    PIC  9(011)V9(02).
                 15 WE01-PDESC-FLEXZ-1     PIC  9(007)V9(04).
              10 WE01-FLEXIB-CPF-CNPF.
                 15 WE01-CINDCD-FLEXZ-2    PIC  X(001).
                 15 WE01-PER-CBR-TARIF-2   PIC  X(001).
                 15 WE01-QFRANQ-FLEXZ-2    PIC  9(003).
                 15 WE01-VNEGOC-FLEXZ-2    PIC  9(011)V9(02).
                 15 WE01-PDESC-FLEXZ-2     PIC  9(007)V9(04).
      *
      ************** FIM DA GFCTWE01 ***********************************
