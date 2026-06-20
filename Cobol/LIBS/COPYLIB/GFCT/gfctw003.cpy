      *---------------------------------------------------------------*
      * NOME DA INC - GFCT0003                                        *
      * DESCRICAO   - ARQUIVO CONTAS EM TEST DRIVE                    *
      *               PROJETO AVON                                    *
      *               LAYOUT UTILIZADO PARA TESTE DRIVE - PARCERIAS   *
      * TAMANHO     - 200                                             *
      * DATA        - 03.09.2019                                      *
      * RESPONSAVEL - MARCIO ALEXANDRO RUI                            *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCT0003-REGISTRO.
           03  GFCT0003-CJUNC-DEPDC                 PIC  9(005).
           03  GFCT0003-CCTA-CORR                   PIC  9(007).
           03  GFCT0003-DINIC-VALDD-SERVC           PIC  X(010).
           03  GFCT0003-DFIM-VALDD-SERVC            PIC  X(010).
           03  GFCT0003-CPARM-GNRCO                 PIC  X(008).
           03  GFCT0003-NOME-CLIENTE                PIC  X(040).
           03  GFCT0003-RAZAO                       PIC  9(005).
ST2507*    03  GFCT0003-CPF-CNPJ                    PIC  9(009).
ST2507*    03  GFCT0003-FILIAL                      PIC  9(005).
ST2507     03  GFCT0003-CPF-CNPJ                    PIC  X(009).
ST2507     03  GFCT0003-FILIAL                      PIC  X(004).
           03  GFCT0003-CONTROLE                    PIC  9(002).
           03  GFCT0003-CESTA-SERVICO               PIC  9(005).
           03  GFCT0003-PRAZO-ISENCAO               PIC  9(003).
           03  GFCT0003-DINIC-TST-DRIVE             PIC  9(008).
           03  GFCT0003-DFIM-TST-DRIVE              PIC  9(008).
RU0620     03  GFCT0003-CSERVICO-SCCE               PIC  9(003).
RU0620*    03  FILLER                               PIC  X(072).
ST2507     03  FILLER                               PIC  X(073).
