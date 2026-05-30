      ******************************************************************
      * NOME BOOK : BVVEW70I - INTERFACE COORDENADOR/FUNCIONAL         *
      * DESCRICAO : MANUTENCAO (INCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : JANEIRO/2022                                       *
      * AUTOR     : MARCELO / MARIANI                                  *
      * TAMANHO   : 14115 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * CAMPOS USADOS SOMENTE NO FUNCIONAL                             *
      *----------------------------------------------------------------*
      * BVVEW70I-CUSUAR               : USUARIO                        *
      * BVVEW70I-TIPO-MANUTENCAO      : TIPO DE MANUNTENCAO            *
      *----------------------------------------------------------------*
      *                    AREA DE ENTRADA                             *
      *----------------------------------------------------------------*
      * BVVEW70I-CCPF                 : CPF/CNPJ                       *
      * BVVEW70I-CFLIAL-CNPJ          : FILIAL CNPJ                    *
      * BVVEW70I-CCTRL-CPF            : CONTROLE CPF/CNPJ              *
      * BVVEW70I-CBCO                 : BANCO                          *
      * BVVEW70I-CAG-BCRIA            : AGENCIA                        *
      * BVVEW70I-CCTA-BCRIA-CLI       : CONTA                          *
      * BVVEW70I-CTPO-CANAL-CADTO     : CODIGO DO CANAL                *
      * BVVEW70I-CINDCD-CADTO-TODO    : INDICADOR CREDENCIADORAS TODAS *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW70I-CINDCD-DOMCL-BCRIO   : DOMICILIADO BANCARIO           *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW70I-QTD-REG              : QUANTIDADE REGISTROS ENVIADOS  *
      * BVVEW70I-CCRDRA-ESTBL-COML    : ID DA CREDENCIADORA            *
      * BVVEW70I-CBANDE-VISAO         : ID DA BANDEIRA                 *
      *----------------------------------------------------------------*
      *                    AREA DE SAIDA                               *
      *----------------------------------------------------------------*
      * BVVEW70I-QTD-REG              : TOTAL DE REGISTROS             *
      * BVVEW70I-CCRDRA-ESTBL-COML    : CODIGO CREDENCIADORA           *
      * BVVEW70I-CBANDE-VISAO         : CODIGO BANDEIRA                *
      * BVVEW70I-QTD-CRED             : TOTAL DE CREDENCIADORAS        *
      * BVVEW70I-TOT-REG              : TOTAL DE REGISTROS             *
      * BVVEW70I-FLAG-GRADE-CIP       : GRADE CIP ABERTA OU FECHADA    *
      *                                 S - SIM - GRADE ABERTA         *
      *                                 N - NAO - GRADE FECHADA        *
      * BVVEW70I-DATA-CONTRATACAO     : DATA DA GRADE CIP              *
      *                                 DD/MM/AAAA                     *
      * BVVEW70I-HORA-CONTRATACAO     : HORA DA GRADE CIP              *
      *                                 HH:MM:SS                       *
      * BVVEW70I-STATUS-ENVIO-CIP     : SITUACAO ENVIO CIP             *
      *                                 0 - A PROCESSAR                *
      *                                 1 - ENVIADO                    *
      *                                 2 - PROCESSADO                 *
      *                                 3 - NAO OK                     *
      * BVVEW70I-DATA-INCLUSAO-REG    : DATA GRAVACAO REGISTRO NA BASE *
      *                                 DD/MM/AAAA HH:MM:SS            *
      ******************************************************************
       05 BVVEW70I-HEADER.
          10 BVVEW70I-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW70I'.
          10 BVVEW70I-TAM-LAYOUT           PIC  9(05) VALUE  14115.

       05 BVVEW70I-REGISTRO-SAIDA.
          10 BVVEW70I-BLOCO-SAIDA.
             15 BVVEW70I-QTD-CRED          PIC  9(04).
             15 BVVEW70I-TOT-REG           PIC  9(04).
             15 BVVEW70I-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW70I-DATA-CONTRATACAO  PIC  X(10).
             15 BVVEW70I-HORA-CONTRATACAO  PIC  X(08).
             15 BVVEW70I-STATUS-ENVIO-CIP  PIC  9(01).
             15 BVVEW70I-DATA-INCLUSAO-REG PIC  X(19).

       05 BVVEW70I-AREA-INTERMEDIARIA.
          10 BVVEW70I-CUSUAR               PIC  X(09).
          10 BVVEW70I-TIPO-MANUTENCAO      PIC  X(01).

       05 BVVEW70I-REGISTRO-ENTRADA.
          10 BVVEW70I-PARTE-FIXA.
             15 BVVEW70I-CCPF              PIC  9(09).
             15 BVVEW70I-CFLIAL-CNPJ       PIC  9(04).
             15 BVVEW70I-CCTRL-CPF         PIC  9(02).
             15 BVVEW70I-CBCO              PIC  9(03).
             15 BVVEW70I-CAG-BCRIA         PIC  9(05).
             15 BVVEW70I-CCTA-BCRIA-CLI    PIC  9(13).
             15 BVVEW70I-CTPO-CANAL-CADTO  PIC  9(03).
             15 BVVEW70I-CINDCD-CADTO-TODO PIC  X(01).
             15 BVVEW70I-CINDCD-DOMCL-BCRIO
                                           PIC  X(01).
          10 BVVEW70I-PARTE-VARIAVEL.
             15 BVVEW70I-QTD-REG           PIC  9(04).
             15 BVVEW70I-TABELA OCCURS  0  TO   2000  TIMES
                DEPENDING ON BVVEW70I-QTD-REG.
                20 BVVEW70I-CCRDRA-ESTBL-COML
                                           PIC  9(04).
                20 BVVEW70I-CBANDE-VISAO   PIC  9(03).
