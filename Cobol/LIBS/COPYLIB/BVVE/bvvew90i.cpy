      ******************************************************************
      * NOME BOOK : BVVEW90I - INTERFACE COORDENADOR/FUNCIONAL         *
      * DESCRICAO : MANUTENCAO (EXCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTOUT)             *
      * DATA      : SETEMBRO/2022                                      *
      * AUTOR     : MARCELO                                            *
      * TAMANHO   : 14110 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                           DESCRICAO                      *
      *----------------------------------------------------------------*
      * CAMPOS USADOS SOMENTE NO FUNCIONAL                             *
      *----------------------------------------------------------------*
      * BVVEW90I-CUSUAR               : USUARIO                        *
      *----------------------------------------------------------------*
      *                    AREA DE ENTRADA                             *
      *----------------------------------------------------------------*
      * BVVEW90I-CCPF                 : CPF/CNPJ                       *
      * BVVEW90I-CFLIAL-CNPJ          : FILIAL CNPJ                    *
      * BVVEW90I-CCTRL-CPF            : CONTROLE CPF/CNPJ              *
      * BVVEW90I-CBCO                 : BANCO                          *
      * BVVEW90I-CAG-BCRIA            : AGENCIA                        *
      * BVVEW90I-CCTA-BCRIA-CLI       : CONTA                          *
      * BVVEW90I-DIG-CCTA-BCRIA       : DIGITO DA CONTA                *
      * BVVEW90I-CTPO-CANAL-CADTO     : CODIGO DO CANAL                *
      * BVVEW90I-CINDCD-CADTO-TODO    : INDICADOR CREDENCIADORAS TODAS *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW90I-CINDCD-DOMCL-BCRIO   : DOMICILIADO BANCARIO           *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW90I-TIPO-MANUTENCAO      : TIPO DE MANUNTENCAO            *
      *                                 I - INCLUSAO                   *
      *                                 E - EXCLUSAO                   *
      * BVVEW90I-QTD-REG              : TOTAL DE REGISTROS             *
      * BVVEW90I-CCRDRA-ESTBL-COML    : ID DA CREDENCIADORA            *
      * BVVEW90I-CBANDE-VISAO         : ID DA BANDEIRA                 *
      *----------------------------------------------------------------*
      *                    AREA DE SAIDA                               *
      *----------------------------------------------------------------*
      * BVVEW90I-QTD-REG              : TOTAL DE REGISTROS             *
      * BVVEW90I-CCRDRA-ESTBL-COML    : CODIGO CREDENCIADORA           *
      * BVVEW90I-CBANDE-VISAO         : CODIGO BANDEIRA                *
      * BVVEW90I-QTD-CRED             : TOTAL DE CREDENCIADORAS        *
      * BVVEW90I-TOT-REG              : TOTAL DE REGISTROS             *
      * BVVEW90I-FLAG-GRADE-CIP       : GRADE CIP ABERTA OU FECHADA    *
      *                                 S - SIM - GRADE ABERTA         *
      *                                 N - NAO - GRADE FECHADA        *
      * BVVEW90I-DATA-CONTRATACAO     : DATA DA GRADE CIP              *
      *                                 DD/MM/AAAA                     *
      * BVVEW90I-HORA-CONTRATACAO     : HORA DA GRADE CIP              *
      *                                 HH:MM:SS                       *
      * BVVEW90I-STATUS-ENVIO-CIP     : SITUACAO ENVIO CIP             *
      *                                 0 - A PROCESSAR                *
      *                                 1 - ENVIADO                    *
      *                                 2 - PROCESSADO                 *
      *                                 3 - NAO OK                     *
      * BVVEW90I-DATA-INCLUSAO-REG    : DATA GRAVACAO REGISTRO NA BASE *
      *                                 DD/MM/AAAA HH:MM:SS            *
      ******************************************************************
       05 BVVEW90I-HEADER.
          10 BVVEW90I-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW90I'.
          10 BVVEW90I-TAM-LAYOUT           PIC  9(05) VALUE  14110.

       05 BVVEW90I-REGISTRO-SAIDA.
          10 BVVEW90I-BLOCO-SAIDA.
             15 BVVEW90I-QTD-CRED          PIC  9(04).
             15 BVVEW90I-TOT-REG           PIC  9(04).
             15 BVVEW90I-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW90I-DATA-CONTRATACAO  PIC  X(10).
             15 BVVEW90I-HORA-CONTRATACAO  PIC  X(08).
             15 BVVEW90I-STATUS-ENVIO-CIP  PIC  9(01).
             15 BVVEW90I-DATA-INCLUSAO-REG PIC  X(19).

       05 BVVEW90I-AREA-INTERMEDIARIA.
          10 BVVEW90I-CUSUAR               PIC  X(09).

       05 BVVEW90I-REGISTRO-ENTRADA.
          10 BVVEW90I-PARTE-FIXA.
             15 BVVEW90I-CCPF              PIC  9(09).
             15 BVVEW90I-CFLIAL-CNPJ       PIC  9(04).
             15 BVVEW90I-CCTRL-CPF         PIC  9(02).
             15 BVVEW90I-CBCO              PIC  9(03).
             15 BVVEW90I-CAG-BCRIA         PIC  9(05).
             15 BVVEW90I-CCTA-BCRIA-CLI    PIC  9(07).
             15 BVVEW90I-DIG-CCTA-BCRIA    PIC  9(01).
             15 BVVEW90I-CTPO-CANAL-CADTO  PIC  9(03).
             15 BVVEW90I-CINDCD-CADTO-TODO PIC  X(01).
             15 BVVEW90I-CINDCD-DOMCL-BCRIO
                                           PIC  X(01).
             15 BVVEW90I-TIPO-MANUTENCAO   PIC  X(01).

          10 BVVEW90I-PARTE-VARIAVEL.
             15 BVVEW90I-QTD-REG           PIC  9(04).
             15 BVVEW90I-TABELA OCCURS  0  TO   2000  TIMES
                DEPENDING ON BVVEW90I-QTD-REG.
                20 BVVEW90I-CCRDRA-ESTBL-COML
                                           PIC  9(04).
                20 BVVEW90I-CBANDE-VISAO   PIC  9(03).
