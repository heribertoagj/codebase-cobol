      ***************************************************************
      * NOME BOOK : I#CCLZ91 - LAYOUT ARQUIVO COBRANCA              *
      * DESCRICAO : ARQUIVO DE DATAS COM PERIODO DE APURACAO PARA   *
      *             ENVIO AO EFIR                                   *
      * DATA      : 11/2019                                         *
      * EMPRESA   : CAPGEMINI                                       *
      * GRUPO     : GRUPO CONTAS                                    *
      * COMPONENTE: CCLZ                                            *
      * TAMANHO   : 00100 BYTES                                     *
      *=============================================================*
      * CCLZ91-DT-INI-ZN         = DATA INICIO ZONADO               *
      * CCLZ91-DT-FIM-ZN         = DATA FIM ZONADO                  *
      * CCLZ91-DT-INI-CP         = DATA INICIO COMPACTADO           *
      * CCLZ91-DT-FIM-CP         = DATA FIM COMPACTADO              *
      * CCLZ91-ANO-MES-ZN        = ANO E MES DE ZONADO              *
      * CCLZ91-ANO-MES-CP        = ANO E MES DE COMPACTADO          *
      ***************************************************************
           05  CCLZ91-DT-INI-ZN           PIC  9(08).
           05  CCLZ91-DT-FIM-ZN           PIC  9(08).
           05  CCLZ91-DT-INI-CP           PIC S9(08) COMP-3.
           05  CCLZ91-DT-FIM-CP           PIC S9(08) COMP-3.
           05  CCLZ91-ANO-MES-ZN          PIC  9(06).
           05  CCLZ91-ANO-MES-CP          PIC S9(06) COMP-3.
           05  FILLER                     PIC  X(64).
