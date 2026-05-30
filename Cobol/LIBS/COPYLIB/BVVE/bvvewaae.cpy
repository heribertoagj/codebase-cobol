      ******************************************************************
      * NOME BOOK : BVVEWAAE - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
VICT  * TAMANHO   : 00049 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWAAE-E-CCPF              : CPF/CNPJ                        *
      * BVVEWAAE-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWAAE-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      *----------------------------------------------------------------*
      *                - - - A L T E R A C O E S - - -                 *
      *----------------------------------------------------------------*
      * DATA..........: FEVEREIRO / 2022          REMARKS....: VICT    *
      * RESPONSAVEL...: VICTORINO                                      *
      * OBJETIVO......: INCLUSAO DOS CAMPOS BANCO / AGENCIA E CONTA    *
      *               BVVEWAAE-E-CBCO              : BANCO             *
      *               BVVEWAAE-E-CAG-BCRIA         : AGENCIA BANCARIA  *
      *               BVVEWAAE-E-CCTA-BCRIA-CLI    : CONTA CORRENTE    *
      ******************************************************************
           05 BVVEWAAE-HEADER.
              10 BVVEWAAE-COD-LAYOUT    PIC  X(08)   VALUE 'BVVEWAAE'.
VICT          10 BVVEWAAE-TAM-LAYOUT    PIC  9(05)   VALUE 00049.
           05 BVVEWAAE-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEWAAE-E-CCPF                  PIC  9(009).
                 15 BVVEWAAE-E-CFLIAL-CNPJ           PIC  9(004).
                 15 BVVEWAAE-E-CCTRL-CPF             PIC  9(002).
VICT             15 BVVEWAAE-E-CBCO                  PIC  9(003).
VICT             15 BVVEWAAE-E-CAG-BCRIA             PIC  9(005).
VICT             15 BVVEWAAE-E-CCTA-BCRIA-CLI        PIC  9(013).
