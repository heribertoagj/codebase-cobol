      ******************************************************************
      * NOME BOOK : BVVEWADE - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTOUT                                        *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
VICT  * TAMANHO   : 00049 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWADE-E-CCPF              : CPF/CNPJ                        *
      * BVVEWADE-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWADE-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      *----------------------------------------------------------------*
      *                - - - A L T E R A C O E S - - -                 *
      *----------------------------------------------------------------*
      * DATA..........: FEVEREIRO / 2022          REMARKS....: VICT    *
      * RESPONSAVEL...: VICTORINO                                      *
      * OBJETIVO......: INCLUSAO DOS CAMPOS BANCO / AGENCIA E CONTA    *
      *               BVVEWADE-E-CBCO              : BANCO             *
      *               BVVEWADE-E-CAG-BCRIA         : AGENCIA BANCARIA  *
      *               BVVEWADE-E-CCTA-BCRIA-CLI    : CONTA CORRENTE    *
      ******************************************************************
           05 BVVEWADE-HEADER.
              10 BVVEWADE-COD-LAYOUT    PIC  X(08)   VALUE 'BVVEWADE'.
VICT          10 BVVEWADE-TAM-LAYOUT    PIC  9(05)   VALUE 00049.
           05 BVVEWADE-REGISTRO.
              10 BVVEWADE-BLOCO-ENTRADA.
                 15 BVVEWADE-E-CCPF                  PIC  9(009).
                 15 BVVEWADE-E-CFLIAL-CNPJ           PIC  9(004).
                 15 BVVEWADE-E-CCTRL-CPF             PIC  9(002).
VICT             15 BVVEWADE-E-CBCO                  PIC  9(003).
VICT             15 BVVEWADE-E-CAG-BCRIA             PIC  9(005).
VICT             15 BVVEWADE-E-CCTA-BCRIA-CLI        PIC  9(013).
