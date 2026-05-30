      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A8 - DB2PRD.THIST_BAIXA_PCELA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/04/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
BRQ001*                    ALTERACAO - BRQ INFORMATICA                *
BRQ001*---------------------------------------------------------------*
BRQ001*  ANALISTA BRQ:   FABIO AUGUSTO FINK      -  BRQ INFORMATICA   *
BRQ001*  DATA........:   08/02/2018                                   *
BRQ001*---------------------------------------------------------------*
BRQ001*  OBJETIVO....:   PROJETO BRQ 17/0418 - TROCAR O CAMPO:        *
BRQ001*            DE: 'CFORMA-PGTO-TITLO' / PARA: 'CORIGE-PGTO-DESC' *
BRQ001*****************************************************************
       01  A8-ARGUMENTOS-ENTRADA.

       05  A8-COLUNAS-DA-TABELA.
           10  A8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  A8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  A8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  A8-HBAIXA-PCELA-DESC            PIC X(26).
           10  A8-HULT-ATULZ                   PIC X(26).
           10  A8-DMES-BAIXA-PTCAO             PIC S9(2)V COMP-3.
           10  A8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  A8-HSIT-DESC-COML               PIC X(26).
           10  A8-CTPO-BAIXA-PCELA             PIC X(1).
           10  A8-CMEIO-PGTO-DESC              PIC S9(3)V COMP-3.
           10  A8-DEFETV-BAIXA-PCELA           PIC X(10).
           10  A8-VSDO-DVDOR-PCELA             PIC S9(15)V9(2) COMP-3.
           10  A8-VPRINC-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8-VJURO-BAIXA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  A8-VIOF-BAIXA-PCELA             PIC S9(15)V9(2) COMP-3.
           10  A8-VMULTA-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8-VABTMT-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8-VDESC-BAIXA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  A8-VEFETV-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8-VJURO-CUSTO-BAIXA            PIC S9(15)V9(2) COMP-3.
           10  A8-VMORA-LEGAL-BAIXA            PIC S9(15)V9(2) COMP-3.
           10  A8-CINDCD-LOC-DEB               PIC X(1).
           10  A8-CINDCD-BAIXA-COBR            PIC X(1).
           10  A8-CINDCD-CONSL-EXTRT           PIC X(1).
BRQ001     10  A8-CORIGE-PGTO-DESC             PIC S9(3)V COMP-3.
           10  A8-CMIDIA-PGTO-TITLO            PIC S9(5)V COMP-3.
           10  A8-CBCO                         PIC S9(3)V COMP-3.
           10  A8-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  A8-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  A8-CDIG-CTA-EXTER               PIC X(2).
           10  A8-CCTRO-CUSTO                  PIC X(4).
           10  A8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A8-CTERM                        PIC X(8).
           10  A8-CTPO-BAIXA-CONTR             PIC X(1).
           10  A8-CTPO-BAIXA-COMPL             PIC X(1).
           10  A8-VDEVLC-BAIXA-EXCED           PIC S9(15)V9(2) COMP-3.
           10  A8-VRECTA-BAIXA-EXCED           PIC S9(15)V9(2) COMP-3.
           10  A8-CBCO-CHEQC                   PIC S9(3)V COMP-3.
           10  A8-CAG-BCRIA-CHEQC              PIC S9(5)V COMP-3.
           10  A8-CCCORR-CHEQC                 PIC S9(13)V COMP-3.
           10  A8-CCHEQ-CHEQC                  PIC S9(7)V COMP-3.
BSI001     10  A8-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  A8-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  A8-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A8-RETORNO.
       05  A8-COD-RETORNO                         PIC 9(04).
       05  A8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A8-PROXIMO-RESTART                     PIC 9(05).
       05  A8-QTDE-RETORNADA                      PIC 9(03).
       05  A8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA8.
           10  A8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  A8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  A8RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  A8RT-HBAIXA-PCELA-DESC          PIC X(26).
           10  A8RT-HULT-ATULZ                 PIC X(26).
           10  A8RT-DMES-BAIXA-PTCAO           PIC S9(2)V COMP-3.
           10  A8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  A8RT-HSIT-DESC-COML             PIC X(26).
           10  A8RT-CTPO-BAIXA-PCELA           PIC X(1).
           10  A8RT-CMEIO-PGTO-DESC            PIC S9(3)V COMP-3.
           10  A8RT-DEFETV-BAIXA-PCELA         PIC X(10).
           10  A8RT-VSDO-DVDOR-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8RT-VPRINC-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  A8RT-VJURO-BAIXA-PCELA          PIC S9(15)V9(2) COMP-3.
           10  A8RT-VIOF-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  A8RT-VMULTA-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  A8RT-VABTMT-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  A8RT-VDESC-BAIXA-PCELA          PIC S9(15)V9(2) COMP-3.
           10  A8RT-VEFETV-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  A8RT-VJURO-CUSTO-BAIXA          PIC S9(15)V9(2) COMP-3.
           10  A8RT-VMORA-LEGAL-BAIXA          PIC S9(15)V9(2) COMP-3.
           10  A8RT-CINDCD-LOC-DEB             PIC X(1).
           10  A8RT-CINDCD-BAIXA-COBR          PIC X(1).
           10  A8RT-CINDCD-CONSL-EXTRT         PIC X(1).
BRQ001     10  A8RT-CORIGE-PGTO-DESC           PIC S9(3)V COMP-3.
           10  A8RT-CMIDIA-PGTO-TITLO          PIC S9(5)V COMP-3.
           10  A8RT-CBCO                       PIC S9(3)V COMP-3.
           10  A8RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  A8RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  A8RT-CDIG-CTA-EXTER             PIC X(2).
           10  A8RT-CCTRO-CUSTO                PIC X(4).
           10  A8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A8RT-CTERM                      PIC X(8).
           10  A8RT-CTPO-BAIXA-CONTR           PIC X(1).
           10  A8RT-CTPO-BAIXA-COMPL           PIC X(1).
           10  A8RT-VDEVLC-BAIXA-EXCED         PIC S9(15)V9(2) COMP-3.
           10  A8RT-VRECTA-BAIXA-EXCED         PIC S9(15)V9(2) COMP-3.
           10  A8RT-CBCO-CHEQC                 PIC S9(3)V COMP-3.
           10  A8RT-CAG-BCRIA-CHEQC            PIC S9(5)V COMP-3.
           10  A8RT-CCCORR-CHEQC               PIC S9(13)V COMP-3.
           10  A8RT-CCHEQ-CHEQC                PIC S9(7)V COMP-3.
BSI001     10  A8RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  A8RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  A8RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
