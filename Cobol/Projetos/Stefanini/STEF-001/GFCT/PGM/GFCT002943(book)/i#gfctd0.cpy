      *****************************************************************
      * NOME DA INC - I#GFCTD0                                        *
      * DESCRICAO   - LAYOUT DO ARQUIVO DE SEGMENTO PARA CONTAS NOVAS *
      * TAMANHO     - 0200                                            *
      * DATA        - 03.2008                                         *
      * RESPONSAVEL - WAGNER - PROCWORK                               *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  D0-REGISTRO.
        02 D0-CADACLIE.
           03  D0-CHAVE.
               05  D0-AGENCIA          PIC  9(05)     COMP-3.
               05  D0-RAZAO            PIC  9(05)     COMP-3.
               05  D0-CONTA            PIC  9(07)     COMP-3.
           03  D0-DIG-CTA              PIC  X(01).
           03  D0-CGC-CPF.
               05  D0-PRINCIPAL        PIC  9(09)     COMP-3.
               05  D0-FILIAL           PIC  9(05)     COMP-3.
               05  D0-CTLE             PIC  9(02).
           03  D0-NOME-CLIE            PIC  X(40).
           03  D0-ENDERECO             PIC  X(40).
           03  D0-CEP.
               05  D0-NRO-CEP          PIC  9(05)     COMP-3.
               05  D0-COMPL-CEP        PIC  9(03).
               05  D0-COMPL-CEP-NULL   REDEFINES
                   D0-COMPL-CEP        PIC  X(03).
           03  D0-TELEFONE.
               05  D0-DDD-FONE         PIC  X(04).
               05  D0-NRO-FONE         PIC  9(09)     COMP-3.
           03  D0-FAX.
               05  D0-DDD-FAX          PIC  X(04).
               05  D0-NRO-FAX          PIC  9(09)     COMP-3.
           03  D0-POSTO-SERV           PIC  9(03)     COMP-3.
           03  D0-RAMO-ATIV            PIC  9(05)     COMP-3.
           03  D0-IDENTF-CTA           PIC  X(02).
           03  D0-DT-ABERT             PIC  9(09)     COMP-3.
           03  D0-DT-ULTMV             PIC  9(09)     COMP-3.
           03  D0-DT-NASCI             PIC  9(09)     COMP-3.
           03  D0-SEXO                 PIC  X(01).
           03  D0-TP-CHEQ              PIC  X(01).
           03  D0-TP-EXTR              PIC  X(01).
           03  D0-RESERVA              PIC  X(30).
        02 D0-SEGMENTO                 PIC  9(03).
        02 D0-TARIFA                   PIC  9(05).
        02 D0-RESERVA2                 PIC  X(12).
ST2507  02 D0-CGC-CPF-ST.
ST2507     03  D0-PRINCIPAL-ST         PIC  X(09).
ST2507     03  D0-FILIAL-ST            PIC  X(04).
ST2507     03  D0-CTLE-ST              PIC  9(02).
